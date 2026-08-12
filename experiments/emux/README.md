# emux MVP

`emux` is a TUI compositor experiment: it starts Emacs and other terminal programs as sibling PTY windows, then lets Emacs control the outer multiplexer through IPC.

This is intentionally small. The MVP validates the architecture, not full tmux compatibility.

## Run

```bash
cargo run -- -- emacs -nw
```

Keys:

- `Ctrl-b n`: open a shell surface
- `Ctrl-b l`: focus Emacs and open `*emux-manager*`
- `Ctrl-b o`: focus next window
- `Ctrl-b Tab`: focus next window
- `Ctrl-b x`: close focused window
- `Ctrl-b q`: quit the compositor
- `Ctrl-b Ctrl-b`: send literal `Ctrl-b` to the focused surface

IPC socket defaults to:

```text
/tmp/emux-$UID.sock
```

Supported line commands:

```text
open-shell
focus-next
close-focused
focus-emacs
open-manager
list-windows
focus-window <id>
rename-window <id> <name>
close-window <id>
new-window <command...>
send <surface-id> <text>
```

`list-windows` returns tab-separated rows:

```text
<id>\t<active:0|1>\t<pid>\t<process>\t<title>
```

The active window renders with a one-line window bar at the bottom:

```text
[0:emacs:emacs]* [1:shell:bash] [2:opencode:opencode]
```

## Emacs Side

Load `elisp/emux.el` inside the Emacs instance managed by `emux`:

```elisp
(load-file "/path/to/experiments/emux/elisp/emux.el")
```

Then use:

```elisp
M-x emux-open-shell
M-x emux-focus-next
M-x emux-close-focused
M-x emux-open-manager
M-x emux-manager
M-x emux-send-region
```

`M-x emux-manager` opens a `tabulated-list-mode` buffer for window management:

- Displays ID, active state, PID, process name, and title.

- `RET` / `j`: focus window
- `n`: create shell window
- `r`: rename window
- `d`: mark window for deletion
- `u`: unmark window
- `x`: close marked windows
- `g`: refresh

The manager refreshes automatically while the `*emux-manager*` buffer is alive.

## MVP Scope

- Outer process owns PTYs and rendering.
- Emacs is one managed PTY surface, not the parent application.
- Shells are sibling PTY windows.
- Windows overlap like tmux windows; only the active window is rendered.
- Emacs controls `emux` over a Unix socket.

## Non-Goals For This Slice

- Persistent detach/attach.
- Scrollback UI.
- Full terminal feature parity.
- Mouse support.
- Config files.
