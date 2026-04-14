# org-opencode

An Emacs package that provides an Org-mode frontend for
[opencode](https://github.com/sst/opencode), an AI coding agent CLI.

org-opencode communicates with the `opencode serve` HTTP API and renders
streaming AI responses -- including tool calls, file edits, and progress
tracking -- directly inside Org buffers.

Use your AI coding agent without leaving Emacs. Responses render as native
Org content.

## Prerequisites

- Emacs 27.1 or later
- [opencode](https://github.com/sst/opencode) CLI installed and configured
  (with at least one AI provider API key)

### Installing opencode

```bash
# Install script
curl -fsSL https://raw.githubusercontent.com/sst/opencode/refs/heads/dev/install.sh | bash

# Homebrew (macOS / Linux)
brew install sst/tap/opencode

# Go install
go install github.com/sst/opencode@latest
```

See the [opencode documentation](https://opencode.ai) for provider
configuration.

## Installation

org-opencode is not yet available on MELPA. Install manually:

```bash
git clone https://github.com/pandaye/org-opencode.git ~/.emacs.d/site-lisp/org-opencode
```

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-opencode")
(require 'org-opencode)

;; Enable in Org buffers
(add-hook 'org-mode-hook #'org-opencode-mode)
```

With `use-package`:

```elisp
(use-package org-opencode
  :load-path "~/.emacs.d/site-lisp/org-opencode"
  :hook (org-mode . org-opencode-mode))
```

## Quick Start

1. Open an Org file.
2. `M-x org-opencode-mode` (or let the hook enable it).
3. Type a prompt in a headline, or select a region.
4. `C-c C-x C-v` to send -- the response streams in real-time below the
   headline.
5. Tool calls appear as collapsible drawers; file edits trigger auto-revert.
6. `C-c C-x C-s` starts a fresh session; `C-c C-x C-l` browses past sessions.

## Key Bindings

All bindings are active when `org-opencode-mode` is enabled.

| Key           | Command                                   | Description                            |
|---------------|-------------------------------------------|----------------------------------------|
| `C-c C-x C-v` | `org-opencode-send`                      | Send prompt (region > headline > minibuffer) |
| `C-c C-x C-s` | `org-opencode-new-session`               | Create new session                     |
| `C-c C-x C-a` | `org-opencode-abort`                     | Abort current work                     |
| `C-c C-x C-e` | `org-opencode-send-as-entry`             | Send as structured Org entry           |
| `C-c C-x C-r` | `org-opencode-adopt-session-from-heading`| Restore session from heading           |
| `C-c C-x C-l` | `org-opencode-list-sessions`             | Browse all sessions                    |
| `C-c C-x C-h` | `org-opencode-session-history`           | View session history                   |
| `C-c C-x C-d` | `org-opencode-show-session-diff`         | View session diff                      |
| `C-c C-x C-m` | `org-opencode-select-model`              | Select AI model                        |
| `C-c C-x C-g` | `org-opencode-select-agent`              | Select agent                           |
| `C-c C-x C-f` | `org-opencode-fork-session`              | Fork current session                   |

## Configuration

```elisp
;; Path to opencode CLI (default: "opencode")
(setq org-opencode-command "opencode")

;; Server address (default: 127.0.0.1:4096)
(setq org-opencode-server-host "127.0.0.1")
(setq org-opencode-server-port 4096)

;; Response layout: 'src-block (default) or 'entry
(setq org-opencode-response-layout 'src-block)

;; Auto-revert files edited by the agent: t, 'ask, 'notify, or nil
(setq org-opencode-auto-revert-files t)

;; Automatically create/load session when mode enables
(setq org-opencode-auto-session-on-mode-enable t)

;; Show status in header line
(setq org-opencode-show-header-status t)

;; Use current headline as default prompt
(setq org-opencode-send-headline-by-default t)
```

## How It Works

1. `org-opencode-mode` starts the opencode CLI server (`opencode serve`) as
   a subprocess.
2. An SSE (Server-Sent Events) connection opens for real-time event
   streaming.
3. Sending a prompt POSTs to the server API; the response streams back as
   SSE events.
4. Events (`message.part.delta`, `tool.updated`, `file.edited`, etc.) are
   dispatched to registered handlers via a plugin-style registry.
5. The render engine writes Org-formatted content incrementally into your
   buffer -- no full-buffer rewrites.

## Architecture

```
org-opencode.el                  Entry point, minor mode, keymap, send command
  |-- org-opencode-core.el       HTTP client, server lifecycle, API wrappers
  |-- org-opencode-events.el     SSE event stream, handler registry, auto-reconnect
  |-- org-opencode-session.el    Session CRUD, file persistence, browser, history
  |-- org-opencode-render.el     Streaming renderer, incremental delta, tool drawers
  |-- org-opencode-approval.el   Permission prompt UI, auto-approve rules
  |-- org-opencode-files.el      File change detection, auto-revert, diff viewer
  +-- org-opencode-ui.el         Model/agent selection, prompt reading
```

## Features

- **Streaming responses** -- AI output renders incrementally as Org content
  (src blocks or entries), not after completion.
- **Tool call visualization** -- each tool invocation appears as a
  collapsible Org drawer with name, arguments, and result.
- **Session management** -- create, fork, browse, adopt, and review session
  history; sessions persist via Org file keywords.
- **File change detection** -- when the agent edits files, affected buffers
  can auto-revert, prompt, or notify; a diff viewer shows all changes.
- **Tool approval UI** -- dangerous operations require explicit approval
  through an Emacs prompt before proceeding.
- **Model and agent selection** -- interactively switch between AI models
  and agents mid-session.
- **Auto-reconnect** -- SSE connection recovers automatically with
  exponential backoff (up to 60 seconds).
- **Progress tracking** -- todo updates from the agent display in real-time.
- **Header-line status** -- a status indicator shows the current session
  state (idle, busy, error).

## Contributing

1. Fork the repository.
2. Create a feature branch (`git checkout -b my-feature`).
3. Make your changes.
4. Byte-compile to check for warnings:
   ```bash
   emacs -Q --batch -L . -f batch-byte-compile org-opencode*.el
   ```
5. Open a pull request.

## License

This project is licensed under the GNU General Public License v3.0 or later.
See [LICENSE](LICENSE) for the full text.

## Acknowledgments

- [opencode](https://github.com/sst/opencode) -- the AI coding agent this
  package interfaces with.
- [Org mode](https://orgmode.org/) -- the backbone of this package's UI.
