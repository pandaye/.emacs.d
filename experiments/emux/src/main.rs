use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::Shutdown;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;
use std::process::Command;
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{anyhow, Context, Result};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use portable_pty::{native_pty_system, Child, CommandBuilder, MasterPty, PtySize};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Position, Rect};
use ratatui::style::{Color as TuiColor, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::{Block, Borders, Paragraph};
use ratatui::Terminal;
use vt100::Color as VtColor;

const FRAME_BUDGET: Duration = Duration::from_millis(8);
const MAX_EVENTS_PER_FRAME: usize = 256;

enum AppEvent {
    Output { id: usize, bytes: Vec<u8> },
    Exited { id: usize },
    Terminal(Event),
    Ipc { command: String, stream: UnixStream },
}

enum PrefixAction {
    Ignored,
    Consumed,
    Quit,
}

struct Surface {
    id: usize,
    title: String,
    kind: SurfaceKind,
    pid: Option<u32>,
    parser: vt100::Parser,
    writer: Box<dyn Write + Send>,
    rows: u16,
    cols: u16,
    diagnostics: SurfaceDiagnostics,
    child: Box<dyn Child + Send + Sync>,
    master: Box<dyn MasterPty + Send>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum SurfaceKind {
    Emacs,
    Terminal,
}

struct SurfaceDiagnostics {
    enabled: bool,
    started_at: Instant,
    first_output_logged: bool,
    first_nonblank_logged: bool,
    first_render_logged: bool,
    start_page_logged: bool,
}

impl SurfaceDiagnostics {
    fn new(enabled: bool) -> Self {
        Self {
            enabled,
            started_at: Instant::now(),
            first_output_logged: false,
            first_nonblank_logged: false,
            first_render_logged: false,
            start_page_logged: false,
        }
    }
}

impl Surface {
    fn new(
        id: usize,
        title: String,
        kind: SurfaceKind,
        command: &[String],
        rows: u16,
        cols: u16,
        tx: Sender<AppEvent>,
    ) -> Result<Self> {
        let pty_system = native_pty_system();
        let pair = pty_system
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .context("open pty")?;

        let mut builder = CommandBuilder::new(&command[0]);
        builder.env("TERM", "xterm-256color");
        builder.env("COLORTERM", "truecolor");
        for arg in &command[1..] {
            builder.arg(arg);
        }

        let child = pair.slave.spawn_command(builder).context("spawn command")?;
        let pid = child.process_id();
        let mut reader = pair.master.try_clone_reader().context("clone pty reader")?;
        let writer = pair.master.take_writer().context("take pty writer")?;

        thread::spawn(move || {
            let mut buf = [0_u8; 8192];
            loop {
                match reader.read(&mut buf) {
                    Ok(0) => {
                        let _ = tx.send(AppEvent::Exited { id });
                        break;
                    }
                    Ok(n) => {
                        if tx
                            .send(AppEvent::Output {
                                id,
                                bytes: buf[..n].to_vec(),
                            })
                            .is_err()
                        {
                            break;
                        }
                    }
                    Err(_) => {
                        let _ = tx.send(AppEvent::Exited { id });
                        break;
                    }
                }
            }
        });

        Ok(Self {
            id,
            title,
            kind,
            pid,
            parser: vt100::Parser::new(rows, cols, 2000),
            writer,
            rows,
            cols,
            diagnostics: SurfaceDiagnostics::new(
                command.first().is_some_and(|cmd| cmd.ends_with("emacs")),
            ),
            child,
            master: pair.master,
        })
    }

    fn resize(&mut self, rows: u16, cols: u16) {
        if rows == 0 || cols == 0 || (self.rows == rows && self.cols == cols) {
            return;
        }

        self.rows = rows;
        self.cols = cols;
        self.parser.set_size(rows, cols);
        let _ = self.master.resize(PtySize {
            rows,
            cols,
            pixel_width: 0,
            pixel_height: 0,
        });
    }

    fn write_all(&mut self, bytes: &[u8]) {
        let _ = self.writer.write_all(bytes);
        let _ = self.writer.flush();
    }

    fn kill(&mut self) {
        let _ = self.child.kill();
    }
}

struct App {
    surfaces: Vec<Surface>,
    focused: usize,
    next_id: usize,
    prefix_pending: bool,
    initial_rows: u16,
    initial_cols: u16,
    tx: Sender<AppEvent>,
}

impl App {
    fn new(tx: Sender<AppEvent>, initial_rows: u16, initial_cols: u16) -> Self {
        Self {
            surfaces: Vec::new(),
            focused: 0,
            next_id: 0,
            prefix_pending: false,
            initial_rows,
            initial_cols,
            tx,
        }
    }

    fn spawn_surface(
        &mut self,
        title: impl Into<String>,
        kind: SurfaceKind,
        command: Vec<String>,
    ) -> Result<()> {
        if command.is_empty() {
            return Err(anyhow!("empty command"));
        }

        let id = self.next_id;
        self.next_id += 1;
        let surface = Surface::new(
            id,
            title.into(),
            kind,
            &command,
            self.initial_rows,
            self.initial_cols,
            self.tx.clone(),
        )?;
        self.surfaces.push(surface);
        self.focused = self.surfaces.len() - 1;
        Ok(())
    }

    fn focused_mut(&mut self) -> Option<&mut Surface> {
        self.surfaces.get_mut(self.focused)
    }

    fn focus_next(&mut self) {
        if !self.surfaces.is_empty() {
            self.focused = (self.focused + 1) % self.surfaces.len();
        }
    }

    fn focus_window(&mut self, id: usize) -> bool {
        let Some(index) = self.surfaces.iter().position(|surface| surface.id == id) else {
            return false;
        };
        self.focused = index;
        true
    }

    fn focus_emacs(&mut self) -> bool {
        let Some(index) = self
            .surfaces
            .iter()
            .position(|surface| surface.kind == SurfaceKind::Emacs)
        else {
            return false;
        };
        self.focused = index;
        true
    }

    fn open_manager(&mut self) -> bool {
        if !self.focus_emacs() {
            return false;
        }
        if let Some(surface) = self.focused_mut() {
            surface.write_all(b"\x1bxemux-manager\r");
            return true;
        }
        false
    }

    fn close_window(&mut self, id: usize) -> bool {
        let Some(index) = self.surfaces.iter().position(|surface| surface.id == id) else {
            return false;
        };
        self.surfaces[index].kill();
        self.remove_surface(id);
        true
    }

    fn rename_window(&mut self, id: usize, name: &str) -> bool {
        let Some(surface) = self.surfaces.iter_mut().find(|surface| surface.id == id) else {
            return false;
        };
        surface.title = name.to_string();
        true
    }

    fn list_windows(&self) -> String {
        self.surfaces
            .iter()
            .enumerate()
            .map(|(index, surface)| {
                format!(
                    "{}\t{}\t{}\t{}\t{}",
                    surface.id,
                    if index == self.focused { 1 } else { 0 },
                    surface.pid.map_or_else(String::new, |pid| pid.to_string()),
                    process_name(surface),
                    surface.title
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn close_focused(&mut self) {
        if let Some(surface) = self.surfaces.get_mut(self.focused) {
            surface.kill();
        }
        if let Some(surface) = self.surfaces.get(self.focused) {
            self.remove_surface(surface.id);
        }
    }

    fn remove_surface(&mut self, id: usize) {
        let Some(index) = self.surfaces.iter().position(|surface| surface.id == id) else {
            return;
        };
        self.surfaces.remove(index);
        if self.surfaces.is_empty() {
            self.focused = 0;
        } else if self.focused >= self.surfaces.len() {
            self.focused = self.surfaces.len() - 1;
        } else if index < self.focused {
            self.focused -= 1;
        }
    }

    fn handle_ipc(&mut self, command: &str) -> String {
        let trimmed = command.trim();
        if trimmed == "focus-next" {
            self.focus_next();
            "OK\n".to_string()
        } else if trimmed == "open-shell" {
            let _ = self.open_shell();
            "OK\n".to_string()
        } else if trimmed == "close-focused" {
            self.close_focused();
            "OK\n".to_string()
        } else if trimmed == "focus-emacs" {
            if self.focus_emacs() {
                "OK\n".to_string()
            } else {
                "ERR emacs window not found\n".to_string()
            }
        } else if trimmed == "open-manager" {
            if self.open_manager() {
                "OK\n".to_string()
            } else {
                "ERR emacs window not found\n".to_string()
            }
        } else if trimmed == "list-windows" {
            format!("{}\n", self.list_windows())
        } else if let Some(rest) = trimmed.strip_prefix("focus-window ") {
            match rest.parse::<usize>() {
                Ok(id) if self.focus_window(id) => "OK\n".to_string(),
                _ => "ERR unknown window\n".to_string(),
            }
        } else if let Some(rest) = trimmed.strip_prefix("rename-window ") {
            let Some((id, name)) = rest.split_once(' ') else {
                return "ERR usage: rename-window <id> <name>\n".to_string();
            };
            match id.parse::<usize>() {
                Ok(id) if self.rename_window(id, name) => "OK\n".to_string(),
                _ => "ERR unknown window\n".to_string(),
            }
        } else if let Some(rest) = trimmed.strip_prefix("close-window ") {
            match rest.parse::<usize>() {
                Ok(id) if self.close_window(id) => "OK\n".to_string(),
                _ => "ERR unknown window\n".to_string(),
            }
        } else if let Some(rest) = trimmed.strip_prefix("new-window ") {
            let command = rest
                .split_whitespace()
                .map(str::to_string)
                .collect::<Vec<_>>();
            match self.spawn_surface(command.join(" "), SurfaceKind::Terminal, command) {
                Ok(()) => "OK\n".to_string(),
                Err(err) => format!("ERR {err}\n"),
            }
        } else if let Some(rest) = trimmed.strip_prefix("send ") {
            self.handle_send(rest);
            "OK\n".to_string()
        } else {
            "ERR unknown command\n".to_string()
        }
    }

    fn handle_send(&mut self, rest: &str) {
        let Some((id, text)) = rest.split_once(' ') else {
            return;
        };
        let Ok(id) = id.parse::<usize>() else {
            return;
        };
        if let Some(surface) = self.surfaces.iter_mut().find(|surface| surface.id == id) {
            surface.write_all(text.as_bytes());
            surface.write_all(b"\n");
        }
    }

    fn open_shell(&mut self) -> Result<()> {
        let shell = env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());
        self.spawn_surface("shell", SurfaceKind::Terminal, vec![shell])
    }
}

struct TerminalGuard;

impl TerminalGuard {
    fn enter() -> Result<Self> {
        enable_raw_mode().context("enable raw mode")?;
        execute!(std::io::stdout(), EnterAlternateScreen).context("enter alternate screen")?;
        Ok(Self)
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = execute!(std::io::stdout(), LeaveAlternateScreen);
    }
}

fn main() -> Result<()> {
    let args = parse_args();
    let socket_path = default_socket_path();
    env::set_var("EMUX_SOCKET", &socket_path);

    let (tx, rx) = mpsc::channel();
    start_ipc_listener(socket_path.clone(), tx.clone())?;

    let _guard = TerminalGuard::enter()?;
    start_terminal_event_reader(tx.clone());
    let backend = CrosstermBackend::new(std::io::stdout());
    let mut terminal = Terminal::new(backend).context("create terminal")?;
    let initial_size = terminal.size().context("read terminal size")?;
    let initial_rows = initial_size.height.saturating_sub(2).max(1);
    let initial_cols = initial_size.width.saturating_sub(2).max(1);

    let mut app = App::new(tx, initial_rows, initial_cols);
    app.spawn_surface("emacs", SurfaceKind::Emacs, args)?;

    run_app(&mut terminal, &mut app, rx)?;

    let _ = fs::remove_file(socket_path);
    Ok(())
}

fn parse_args() -> Vec<String> {
    let mut args: Vec<String> = env::args().skip(1).collect();
    if args.first().map(String::as_str) == Some("--") {
        args.remove(0);
    }
    if args.is_empty() {
        vec!["emacs".to_string(), "-nw".to_string()]
    } else {
        args
    }
}

fn default_socket_path() -> PathBuf {
    if let Ok(path) = env::var("EMUX_SOCKET") {
        return PathBuf::from(path);
    }
    let uid =
        env::var("UID").unwrap_or_else(|_| current_uid().unwrap_or_else(|| "unknown".to_string()));
    PathBuf::from(format!("/tmp/emux-{uid}.sock"))
}

fn current_uid() -> Option<String> {
    let output = Command::new("id").arg("-u").output().ok()?;
    if !output.status.success() {
        return None;
    }
    let uid = String::from_utf8(output.stdout).ok()?;
    Some(uid.trim().to_string())
}

fn start_ipc_listener(path: PathBuf, tx: Sender<AppEvent>) -> Result<()> {
    let _ = fs::remove_file(&path);
    let listener = UnixListener::bind(&path).with_context(|| format!("bind {}", path.display()))?;
    thread::spawn(move || {
        for mut stream in listener.incoming().flatten() {
            let Ok(reader_stream) = stream.try_clone() else {
                let _ = stream.write_all(b"ERR failed to clone IPC stream\n");
                continue;
            };
            let mut reader = BufReader::new(reader_stream);
            let mut command = String::new();
            if reader.read_line(&mut command).is_ok()
                && tx.send(AppEvent::Ipc { command, stream }).is_err()
            {
                return;
            }
        }
    });
    Ok(())
}

fn start_terminal_event_reader(tx: Sender<AppEvent>) {
    thread::spawn(move || {
        while let Ok(event) = event::read() {
            if tx.send(AppEvent::Terminal(event)).is_err() {
                break;
            }
        }
    });
}

fn run_app(
    terminal: &mut Terminal<CrosstermBackend<std::io::Stdout>>,
    app: &mut App,
    rx: Receiver<AppEvent>,
) -> Result<()> {
    loop {
        if let Ok(event) = rx.recv_timeout(Duration::from_millis(16)) {
            if handle_app_event(app, event) {
                break;
            }
        }
        if drain_app_events(app, &rx, FRAME_BUDGET, MAX_EVENTS_PER_FRAME) {
            break;
        }
        if app.surfaces.is_empty() {
            break;
        }
        terminal.draw(|frame| render(frame, app))?;
    }
    Ok(())
}

fn handle_prefix_key(app: &mut App, key: KeyEvent) -> PrefixAction {
    if app.prefix_pending {
        app.prefix_pending = false;
        match key.code {
            KeyCode::Char('n') => {
                let _ = app.open_shell();
            }
            KeyCode::Char('o') | KeyCode::Tab => app.focus_next(),
            KeyCode::Char('l') => {
                app.open_manager();
            }
            KeyCode::Char('x') => app.close_focused(),
            KeyCode::Char('q') => return PrefixAction::Quit,
            KeyCode::Char('b') if is_prefix_key(key) => {
                if let Some(surface) = app.focused_mut() {
                    surface.write_all(&[0x02]);
                }
            }
            _ => {}
        }
        return PrefixAction::Consumed;
    }

    if is_prefix_key(key) {
        app.prefix_pending = true;
        return PrefixAction::Consumed;
    }

    PrefixAction::Ignored
}

fn is_prefix_key(key: KeyEvent) -> bool {
    key.code == KeyCode::Char('b') && key.modifiers.contains(KeyModifiers::CONTROL)
}

fn drain_app_events(
    app: &mut App,
    rx: &Receiver<AppEvent>,
    budget: Duration,
    max_events: usize,
) -> bool {
    let started_at = Instant::now();
    for _ in 0..max_events {
        if started_at.elapsed() >= budget {
            break;
        }
        let Ok(event) = rx.try_recv() else {
            break;
        };
        if handle_app_event(app, event) {
            return true;
        }
    }
    false
}

fn handle_app_event(app: &mut App, event: AppEvent) -> bool {
    match event {
        AppEvent::Output { id, bytes } => {
            if let Some(surface) = app.surfaces.iter_mut().find(|surface| surface.id == id) {
                log_first_output(surface);
                surface.parser.process(&bytes);
                respond_to_terminal_queries(surface, &bytes);
                log_screen_milestones(surface);
            }
        }
        AppEvent::Exited { id } => app.remove_surface(id),
        AppEvent::Terminal(Event::Key(key)) => {
            match handle_prefix_key(app, key) {
                PrefixAction::Quit => return true,
                PrefixAction::Consumed => return false,
                PrefixAction::Ignored => {}
            }
            if let Some(bytes) = encode_key(key) {
                if let Some(surface) = app.focused_mut() {
                    surface.write_all(&bytes);
                }
            }
        }
        AppEvent::Terminal(Event::Resize(_, _)) => {}
        AppEvent::Terminal(_) => {}
        AppEvent::Ipc {
            command,
            mut stream,
        } => {
            let response = app.handle_ipc(&command);
            let _ = stream.write_all(response.as_bytes());
            let _ = stream.flush();
            let _ = stream.shutdown(Shutdown::Both);
        }
    }
    false
}

fn respond_to_terminal_queries(surface: &mut Surface, bytes: &[u8]) {
    if contains_subsequence(bytes, b"\x1b[6n") {
        let (row, col) = surface.parser.screen().cursor_position();
        surface.write_all(format!("\x1b[{};{}R", row + 1, col + 1).as_bytes());
        log_diag(&format!("{} answered CPR", surface.title));
    }

    if contains_subsequence(bytes, b"\x1b[c") || contains_subsequence(bytes, b"\x1b[0c") {
        surface.write_all(b"\x1b[?1;2c");
        log_diag(&format!("{} answered primary DA", surface.title));
    }

    if contains_subsequence(bytes, b"\x1b[>c") || contains_subsequence(bytes, b"\x1b[>0c") {
        surface.write_all(b"\x1b[>0;0;0c");
        log_diag(&format!("{} answered secondary DA", surface.title));
    }
}

fn contains_subsequence(haystack: &[u8], needle: &[u8]) -> bool {
    !needle.is_empty()
        && haystack
            .windows(needle.len())
            .any(|window| window == needle)
}

fn process_name(surface: &Surface) -> String {
    let Some(pid) = surface.pid else {
        return "unknown".to_string();
    };
    fs::read_to_string(format!("/proc/{pid}/comm"))
        .map(|name| name.trim().to_string())
        .ok()
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "unknown".to_string())
}

fn render(frame: &mut ratatui::Frame<'_>, app: &mut App) {
    if app.surfaces.is_empty() {
        return;
    }

    let areas = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(1), Constraint::Length(1)])
        .split(frame.area());

    let Some(surface) = app.surfaces.get_mut(app.focused) else {
        return;
    };
    let title = format!(" {}:{} ", surface.id, surface.title);
    let style = Style::default()
        .fg(TuiColor::Cyan)
        .add_modifier(Modifier::BOLD);
    let block = Block::default()
        .borders(Borders::ALL)
        .title(title)
        .border_style(style);
    let area = areas[0];
    let inner = block.inner(area);
    surface.resize(inner.height, inner.width);
    let lines = surface_lines(surface, inner);
    frame.render_widget(Paragraph::new(lines).block(block), area);
    log_first_render(surface);
    set_surface_cursor(frame, surface, inner);
    render_window_bar(frame, app, areas[1]);
}

fn render_window_bar(frame: &mut ratatui::Frame<'_>, app: &App, area: Rect) {
    let spans = app
        .surfaces
        .iter()
        .enumerate()
        .flat_map(|(index, surface)| {
            let label = format!(
                "[{}:{}:{}]{} ",
                surface.id,
                surface.title,
                process_name(surface),
                if index == app.focused { "*" } else { "" }
            );
            let style = if index == app.focused {
                Style::default()
                    .fg(TuiColor::Black)
                    .bg(TuiColor::Cyan)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(TuiColor::Gray)
            };
            [Span::styled(label, style)].into_iter()
        })
        .collect::<Vec<_>>();

    frame.render_widget(Paragraph::new(Text::from(Line::from(spans))), area);
}

fn log_first_output(surface: &mut Surface) {
    if !surface.diagnostics.enabled || surface.diagnostics.first_output_logged {
        return;
    }
    surface.diagnostics.first_output_logged = true;
    log_diag(&format!(
        "{} first PTY output at {:.3}s",
        surface.title,
        surface.diagnostics.started_at.elapsed().as_secs_f64()
    ));
}

fn log_screen_milestones(surface: &mut Surface) {
    if !surface.diagnostics.enabled {
        return;
    }

    let contents = surface.parser.screen().contents();
    if !surface.diagnostics.first_nonblank_logged && !contents.trim().is_empty() {
        surface.diagnostics.first_nonblank_logged = true;
        log_diag(&format!(
            "{} first nonblank screen at {:.3}s",
            surface.title,
            surface.diagnostics.started_at.elapsed().as_secs_f64()
        ));
    }

    if !surface.diagnostics.start_page_logged && contents.contains("SLOW IS FAST") {
        surface.diagnostics.start_page_logged = true;
        log_diag(&format!(
            "{} start-page marker at {:.3}s",
            surface.title,
            surface.diagnostics.started_at.elapsed().as_secs_f64()
        ));
    }
}

fn log_first_render(surface: &mut Surface) {
    if !surface.diagnostics.enabled || surface.diagnostics.first_render_logged {
        return;
    }
    surface.diagnostics.first_render_logged = true;
    log_diag(&format!(
        "{} first render at {:.3}s",
        surface.title,
        surface.diagnostics.started_at.elapsed().as_secs_f64()
    ));
}

fn log_diag(message: &str) {
    let Ok(path) = env::var("EMUX_DIAG") else {
        return;
    };
    if let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) {
        let _ = writeln!(file, "emux diag {message}");
    }
}

fn set_surface_cursor(frame: &mut ratatui::Frame<'_>, surface: &Surface, area: Rect) {
    let screen = surface.parser.screen();
    if screen.hide_cursor() || area.is_empty() {
        return;
    }

    let (row, col) = screen.cursor_position();
    let row = row.min(area.height.saturating_sub(1));
    let col = col.min(area.width.saturating_sub(1));
    frame.set_cursor_position(Position {
        x: area.x + col,
        y: area.y + row,
    });
}

fn surface_lines(surface: &Surface, area: Rect) -> Vec<Line<'static>> {
    let screen = surface.parser.screen();
    let mut lines = Vec::with_capacity(area.height as usize);

    for row in 0..area.height {
        let mut spans = Vec::with_capacity(area.width as usize);
        for col in 0..area.width {
            let Some(cell) = screen.cell(row, col) else {
                spans.push(Span::raw(" "));
                continue;
            };
            if cell.is_wide_continuation() {
                continue;
            }

            let text = if cell.has_contents() {
                cell.contents()
            } else {
                " ".to_string()
            };
            spans.push(Span::styled(text, cell_style(cell)));
        }
        lines.push(Line::from(spans));
    }

    lines
}

fn cell_style(cell: &vt100::Cell) -> Style {
    let (fg, bg) = if cell.inverse() {
        (to_tui_color(cell.bgcolor()), to_tui_color(cell.fgcolor()))
    } else {
        (to_tui_color(cell.fgcolor()), to_tui_color(cell.bgcolor()))
    };

    let mut style = Style::default();
    if let Some(color) = fg {
        style = style.fg(color);
    }
    if let Some(color) = bg {
        style = style.bg(color);
    }

    let mut modifier = Modifier::empty();
    if cell.bold() {
        modifier |= Modifier::BOLD;
    }
    if cell.italic() {
        modifier |= Modifier::ITALIC;
    }
    if cell.underline() {
        modifier |= Modifier::UNDERLINED;
    }

    style.add_modifier(modifier)
}

fn to_tui_color(color: VtColor) -> Option<TuiColor> {
    match color {
        VtColor::Default => None,
        VtColor::Idx(index) => Some(match index {
            0 => TuiColor::Black,
            1 => TuiColor::Red,
            2 => TuiColor::Green,
            3 => TuiColor::Yellow,
            4 => TuiColor::Blue,
            5 => TuiColor::Magenta,
            6 => TuiColor::Cyan,
            7 => TuiColor::Gray,
            8 => TuiColor::DarkGray,
            9 => TuiColor::LightRed,
            10 => TuiColor::LightGreen,
            11 => TuiColor::LightYellow,
            12 => TuiColor::LightBlue,
            13 => TuiColor::LightMagenta,
            14 => TuiColor::LightCyan,
            15 => TuiColor::White,
            index => TuiColor::Indexed(index),
        }),
        VtColor::Rgb(r, g, b) => Some(TuiColor::Rgb(r, g, b)),
    }
}

fn encode_key(key: KeyEvent) -> Option<Vec<u8>> {
    let mut bytes = encode_key_without_meta(key)?;
    if key.modifiers.contains(KeyModifiers::ALT) {
        bytes.insert(0, 0x1b);
    }
    Some(bytes)
}

fn encode_key_without_meta(key: KeyEvent) -> Option<Vec<u8>> {
    match key.code {
        KeyCode::Char(c) if key.modifiers.contains(KeyModifiers::CONTROL) => {
            let lower = c.to_ascii_lowercase() as u8;
            if lower >= b'a' && lower <= b'z' {
                Some(vec![lower - b'a' + 1])
            } else {
                None
            }
        }
        KeyCode::Char(c) => Some(c.to_string().into_bytes()),
        KeyCode::Enter => Some(b"\r".to_vec()),
        KeyCode::Backspace => Some(vec![0x7f]),
        KeyCode::Esc => Some(vec![0x1b]),
        KeyCode::Tab => Some(b"\t".to_vec()),
        KeyCode::Left => Some(b"\x1b[D".to_vec()),
        KeyCode::Right => Some(b"\x1b[C".to_vec()),
        KeyCode::Up => Some(b"\x1b[A".to_vec()),
        KeyCode::Down => Some(b"\x1b[B".to_vec()),
        KeyCode::Home => Some(b"\x1b[H".to_vec()),
        KeyCode::End => Some(b"\x1b[F".to_vec()),
        KeyCode::Delete => Some(b"\x1b[3~".to_vec()),
        _ => None,
    }
}
