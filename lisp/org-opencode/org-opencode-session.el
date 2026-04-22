;;; org-opencode-session.el --- Session management for org-opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 pandaye

;; Author: pandaye
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, ai, tools
;; URL: https://github.com/pandaye/org-opencode

;; This file is part of org-opencode.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides session management functionality for org-opencode.
;; It handles session lifecycle (create, adopt, reset), file-level session
;; persistence, and provides a session browser interface.
;;
;; Features:
;; - Buffer-local session tracking
;; - Session persistence via Org file keywords
;; - Session adoption from headings (for archived conversations)
;; - Session list buffer with browsing and deletion
;; - Session history viewer
;; - Session forking

;;; Code:

(require 'org)
(require 'org-opencode-core)

;; Customization

(defgroup org-opencode-session nil
  "Session management for org-opencode."
  :group 'org-opencode
  :prefix "org-opencode-")

(defcustom org-opencode-session-id-property "OPENCODE_SESSION_ID"
  "Org property name used to store opencode session id in entries.

When a structured entry is created, this property is set on the
heading to allow later adoption via `org-opencode-adopt-session-from-heading'."
  :type 'string
  :group 'org-opencode-session)

(defcustom org-opencode-file-session-keyword "OPENCODE_SESSION_ID"
  "File keyword used to persist the current opencode session id.

When auto session mode is enabled, this keyword is read when
`org-opencode-mode' turns on, and updated when a new session is created."
  :type 'string
  :group 'org-opencode-session)

(defcustom org-opencode-file-directory-keyword "OPENCODE_DIR"
  "File keyword used to persist the project directory for opencode.

When `org-opencode-mode' turns on, this keyword is read.  If absent,
the user is prompted via `read-directory-name' and the chosen path
is written back to the file."
  :type 'string
  :group 'org-opencode-session)

(defcustom org-opencode-auto-session-on-mode-enable t
  "When non-nil, auto-load or auto-create a session on mode enable.

If `org-opencode-file-session-keyword' exists in the current Org file,
its session id is adopted. Otherwise, a new session is created and written
back to the file keyword."
  :type 'boolean
  :group 'org-opencode-session)

;; Buffer-local variables

(defvar-local org-opencode--session nil
  "Buffer-local session object returned by the opencode HTTP API.

This is an alist containing session metadata including:
- id: the session identifier
- title: the session title
- created_at: creation timestamp
- summary: session statistics (additions, deletions, files)
- directory: project directory")

;; Internal functions

(defun org-opencode--session-id ()
  "Return the current buffer session id, or nil."
  (alist-get 'id org-opencode--session))

(defun org-opencode--file-keyword-value (keyword)
  "Return first file-level KEYWORD value from current Org buffer."
  (let* ((key (upcase keyword))
         (alist (org-collect-keywords (list key))))
    (car (cdr (assoc key alist)))))

(defun org-opencode--set-file-keyword (keyword value)
  "Set file-level KEYWORD to VALUE in current Org buffer.

If the keyword already exists, its value is updated in place.
Otherwise, the keyword is inserted after the last existing keyword line."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((key (upcase keyword))
             (line (format "#+%s: %s" key value))
             (regexp (format "^#\\+%s:[[:space:]]*.*$" (regexp-quote key)))
             (case-fold-search t))
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            (replace-match line t t)
          (goto-char (point-min))
          (while (looking-at "^#\\+[A-Za-z0-9_@%#-]+:")
            (forward-line 1))
          (unless (bolp)
            (insert "\n"))
          (insert line "\n"))))))

(defun org-opencode--adopt-session-id (session-id)
  "Adopt SESSION-ID into current buffer-local opencode state.

This sets `org-opencode--session' to a minimal alist containing
just the id, suitable for session resumption."
  (setq org-opencode--session `((id . ,session-id)))
  session-id)

(defun org-opencode--remember-session-in-file (&optional session-id)
  "Persist SESSION-ID to file keyword in current Org buffer.

If SESSION-ID is nil, use the current buffer's session id.
This function is a no-op if the buffer is not in org-mode."
  (let ((sid (or session-id (org-opencode--session-id))))
    (when (and sid (not (string-empty-p sid)) (derived-mode-p 'org-mode))
      (org-opencode--set-file-keyword org-opencode-file-session-keyword sid))))

(defun org-opencode--load-session-from-file ()
  "Load session id from file keyword and adopt it.

Return adopted session id, or nil when absent or buffer is not
in org-mode."
  (when (derived-mode-p 'org-mode)
    (let ((sid (org-opencode--file-keyword-value org-opencode-file-session-keyword)))
      (when (and sid (not (string-empty-p sid)))
        (org-opencode--adopt-session-id sid)))))

(defun org-opencode--maybe-auto-session ()
  "Auto-load or auto-create session for current Org buffer when enabled.

If `org-opencode-auto-session-on-mode-enable' is non-nil and the
buffer is in org-mode:
1. Try to load existing session from file keyword
2. If no session found, create a new one with default title
3. Remember the new session in the file keyword

Returns the session alist, or nil if auto-session is disabled."
  (when (and org-opencode-auto-session-on-mode-enable
             (derived-mode-p 'org-mode)
             (not (org-opencode--session-id)))
    (or (org-opencode--load-session-from-file)
        (progn
          (org-opencode-new-session (funcall org-opencode-session-title-function))
          (org-opencode--remember-session-in-file))))
  org-opencode--session)

;; Public functions

(defun org-opencode-new-session (&optional title)
  "Create a new opencode session for the current Org buffer.

If TITLE is not provided, use the value from
`org-opencode-session-title-function'.

The new session is stored in `org-opencode--session' and persisted
to the file keyword if in org-mode."
  (interactive
   (list
    (let ((default (funcall org-opencode-session-title-function)))
      (read-string "Session title: " default nil default))))
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-opencode-new-session' only works in Org buffers"))
  (org-opencode--ensure-server)
  (setq org-opencode--session
        (org-opencode--http-json
         "POST"
         (org-opencode--path-with-query "/session" (org-opencode--session-query))
         `((title . ,title))))
  (org-opencode--remember-session-in-file)
  (message "Created opencode session %s" (org-opencode--session-id)))

(defun org-opencode--ensure-session ()
  "Ensure the current Org buffer has an opencode session.

If no session exists, create one with the default title.
Returns the session alist."
  (unless (org-opencode--session-id)
    (org-opencode-new-session (funcall org-opencode-session-title-function)))
  org-opencode--session)

(defun org-opencode-adopt-session-from-heading ()
  "Adopt the session id stored at current Org heading.

This allows resuming a previous opencode conversation from archived
entries that contain `org-opencode-session-id-property'.

The property value is copied into `org-opencode--session' for use
in subsequent operations."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-opencode-adopt-session-from-heading' only works in Org buffers"))
  (let ((session-id (org-entry-get nil org-opencode-session-id-property t)))
    (unless (and session-id (not (string-empty-p session-id)))
      (user-error "No %s property found at current heading" org-opencode-session-id-property))
    (org-opencode--adopt-session-id session-id)
    (message "Adopted opencode session %s" session-id)))

(defun org-opencode-reset-session ()
  "Forget the current buffer-local opencode session.

This clears `org-opencode--session' but does not delete the session
from the opencode server. Use `org-opencode-delete-session' to
permanently remove a session."
  (interactive)
  (setq org-opencode--session nil)
  (message "Cleared current opencode session"))

;; Session List Buffer

(defconst org-opencode--sessions-buffer-name "*opencode-sessions*"
  "Name of the buffer for browsing opencode sessions.")

(defvar org-opencode-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-opencode-sessions-adopt)
    (define-key map (kbd "d") #'org-opencode-sessions-delete)
    (define-key map (kbd "s") #'org-opencode-sessions-switch-to-directory)
    (define-key map (kbd "g") #'org-opencode-list-sessions)
    (define-key map (kbd "h") #'org-opencode-sessions-history)
    (define-key map (kbd "f") #'org-opencode-sessions-fork)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-opencode-sessions-mode'.

\<org-opencode-sessions-mode-map>
\{org-opencode-sessions-mode-map}")

(define-derived-mode org-opencode-sessions-mode special-mode "OpenCode Sessions"
  "Major mode for browsing opencode sessions.

This mode displays all opencode sessions in a tabular format with
the following columns:
- Title: the session title
- ID: truncated session identifier
- Directory: project directory
- Created: formatted creation time
- Stats: additions/deletions/files

Commands:
- RET (`org-opencode-sessions-adopt'): adopt session at point
- d (`org-opencode-sessions-delete'): delete session at point
- s (`org-opencode-sessions-switch-to-directory'): switch to session directory
- g (`org-opencode-list-sessions'): refresh list
- h (`org-opencode-sessions-history'): view session history
- f (`org-opencode-sessions-fork'): fork session at point
- q (`quit-window'): quit buffer"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun org-opencode--format-session-time (timestamp)
  "Format TIMESTAMP for display in session list.

TIMESTAMP is expected in ISO 8601 format. Returns a human-readable
string like \"2025-04-09 14:30\"."
  (if (not timestamp)
      "Unknown"
    (condition-case nil
        (let ((time (encode-time (parse-time-string timestamp))))
          (format-time-string "%Y-%m-%d %H:%M" time))
      (error "Invalid"))))

(defun org-opencode--truncate-session-id (session-id)
  "Truncate SESSION-ID for display.

Shows first 8 characters followed by ellipsis."
  (if (and session-id (> (length session-id) 8))
      (concat (substring session-id 0 8) "…")
    (or session-id "N/A")))

(defun org-opencode--format-session-stats (session)
  "Format statistics from SESSION alist.

Returns a string showing additions/deletions/files, or empty string
if no summary available."
  (let* ((summary (alist-get 'summary session))
         (additions (alist-get 'additions summary))
         (deletions (alist-get 'deletions summary))
         (files (alist-get 'files summary)))
    (if summary
        (format "+%d/-%d/%d"
                (or additions 0)
                (or deletions 0)
                (or files 0))
      "")))

(defun org-opencode--insert-session-line (session)
  "Insert a line representing SESSION in the sessions buffer.

Uses button properties for interactive navigation."
  (let* ((id (alist-get 'id session))
         (title (or (alist-get 'title session) "Untitled"))
         (directory (or (alist-get 'directory session) ""))
         (created (alist-get 'created_at session))
         (stats (org-opencode--format-session-stats session)))
    (insert-button
     title
     'action #'org-opencode-sessions-adopt
     'session-id id
     'follow-link t
     'help-echo (format "Session %s" id))
    (indent-to 40)
    (insert (org-opencode--truncate-session-id id))
    (indent-to 52)
    (insert-button
     directory
     'action #'org-opencode-sessions-switch-to-directory
     'session-id id
     'directory directory
     'follow-link t
     'help-echo "Click to switch to this directory")
    (indent-to 92)
    (insert (org-opencode--format-session-time created))
    (indent-to 110)
    (insert stats)
    (insert "\n")))

(defun org-opencode-list-sessions ()
  "List all opencode sessions in a dedicated buffer.

Creates or reuses `org-opencode--sessions-buffer-name' and displays
all sessions with their metadata. Sessions are sorted by creation time
(newest first).

Key bindings in the session list:
- RET: adopt session into current org buffer
- d: delete session
- s: switch to session directory
- g: refresh list
- h: view session history
- f: fork session"
  (interactive)
  (org-opencode--ensure-server)
  (let* ((sessions (org-opencode-api-sessions (org-opencode--session-query)))
         (sorted-sessions (sort sessions
                                (lambda (a b)
                                  (string> (or (alist-get 'created_at a) "")
                                           (or (alist-get 'created_at b) "")))))
         (buffer (get-buffer-create org-opencode--sessions-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "OpenCode Sessions\n")
        (insert (make-string 120 ?-))
        (insert "\n")
        (insert (propertize "Title" 'face 'bold))
        (indent-to 40)
        (insert (propertize "ID" 'face 'bold))
        (indent-to 52)
        (insert (propertize "Directory" 'face 'bold))
        (indent-to 92)
        (insert (propertize "Created" 'face 'bold))
        (indent-to 110)
        (insert (propertize "Stats" 'face 'bold))
        (insert "\n")
        (insert (make-string 120 ?-))
        (insert "\n")
        (if sorted-sessions
            (dolist (session sorted-sessions)
              (org-opencode--insert-session-line session))
          (insert "No sessions found for current directory.\n"))
        (insert (make-string 120 ?-))
        (insert "\n")
        (insert "RET: adopt | d: delete | s: switch dir | g: refresh | h: history | f: fork | q: quit")
        (goto-char (point-min))
        (forward-line 2)))
    (pop-to-buffer buffer)
    (org-opencode-sessions-mode)))

(defun org-opencode-sessions-adopt (&optional button)
  "Adopt the session at point or from BUTTON into the current Org buffer.

If called from the sessions buffer, uses the session at point.
Otherwise, prompts for a session ID."
  (interactive)
  (let ((session-id (if button
                        (button-get button 'session-id)
                      (when (derived-mode-p 'org-opencode-sessions-mode)
                        (get-text-property (point) 'session-id)))))
    (if (not session-id)
        (call-interactively #'org-opencode-adopt-session-by-id)
      (if (derived-mode-p 'org-mode)
          (progn
            (org-opencode--adopt-session-id session-id)
            (message "Adopted session %s" session-id))
        (message "Session %s selected (switch to an Org buffer to adopt)" session-id)))))

(defun org-opencode-adopt-session-by-id (session-id)
  "Adopt SESSION-ID into the current Org buffer.

Prompts for the session ID interactively."
  (interactive "sSession ID to adopt: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Can only adopt sessions in Org buffers"))
  (when (string-empty-p session-id)
    (user-error "Session ID cannot be empty"))
  (org-opencode--adopt-session-id session-id)
  (org-opencode--remember-session-in-file)
  (message "Adopted session %s" session-id))

(defun org-opencode-sessions-delete (&optional button)
  "Delete the session at point or from BUTTON.

Asks for confirmation before deletion. If the deleted session is
currently adopted in any buffer, those buffers will have their
session cleared."
  (interactive)
  (let ((session-id (if button
                        (button-get button 'session-id)
                      (get-text-property (point) 'session-id))))
    (when (not session-id)
      (user-error "No session at point"))
    (when (yes-or-no-p (format "Delete session %s? " session-id))
      (org-opencode-delete-session session-id)
      (when (derived-mode-p 'org-opencode-sessions-mode)
        (org-opencode-list-sessions)))))

(defun org-opencode-delete-session (session-id)
  "Delete an opencode session by SESSION-ID.

This permanently removes the session and all its messages from the
opencode server. Buffers that had this session adopted will show
\"Cleared current opencode session\" on their next operation."
  (interactive "sSession ID to delete: ")
  (when (string-empty-p session-id)
    (user-error "Session ID cannot be empty"))
  (org-opencode--ensure-server)
  (org-opencode-api-delete-session session-id)
  ;; Clear session from current buffer if it matches
  (when (equal (org-opencode--session-id) session-id)
    (setq org-opencode--session nil))
  (message "Deleted session %s" session-id))

(defun org-opencode-sessions-switch-to-directory (&optional button)
  "Switch to the directory of the session at point or from BUTTON.

Uses `dired' to open the session's project directory."
  (interactive)
  (let ((directory (if button
                       (button-get button 'directory)
                     (get-text-property (point) 'directory))))
    (when (not directory)
      (user-error "No directory associated with this session"))
    (when (not (file-directory-p directory))
      (user-error "Directory does not exist: %s" directory))
    (dired directory)))

;; Session History

(defconst org-opencode--history-buffer-name "*opencode-history*"
  "Name of the buffer for viewing session history.")

(defvar org-opencode-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-opencode-history-mode'.")

(define-derived-mode org-opencode-history-mode special-mode "OpenCode History"
  "Major mode for viewing opencode session message history.

Displays all messages in a session with their metadata including
role, timestamp, model used, and token counts.

Commands:
- q (`quit-window'): quit buffer"
  (setq truncate-lines nil)
  (setq buffer-read-only t))

(defun org-opencode--format-message-role (role)
  "Format ROLE for display in history buffer."
  (pcase role
    ("user" (propertize "User" 'face 'font-lock-keyword-face))
    ("assistant" (propertize "Assistant" 'face 'font-lock-function-name-face))
    ("system" (propertize "System" 'face 'font-lock-comment-face))
    (_ (propertize (or role "Unknown") 'face 'font-lock-type-face))))

(defun org-opencode--format-message-parts-summary (parts)
  "Create a summary of PARTS for display.

Shows the number of text parts and any tool calls."
  (let ((text-count 0)
        (tool-count 0))
    (dolist (part parts)
      (pcase (alist-get 'type part)
        ("text" (setq text-count (1+ text-count)))
        ("tool" (setq tool-count (1+ tool-count)))))
    (cond
     ((and (> text-count 0) (> tool-count 0))
      (format "%d text, %d tools" text-count tool-count))
     ((> text-count 0)
      (format "%d text" text-count))
     ((> tool-count 0)
      (format "%d tools" tool-count))
     (t "empty"))))

(defun org-opencode--insert-message (message)
  "Insert MESSAGE into the history buffer.

MESSAGE is an alist containing message metadata and parts."
  (let* ((_id (alist-get 'id message))
         (role (alist-get 'role message))
         (info (alist-get 'info message))
         (parts (alist-get 'parts message))
         (model (alist-get 'model info))
         (timestamp (alist-get 'created_at info))
         (tokens (alist-get 'usage info))
         (input-tokens (alist-get 'input tokens))
         (output-tokens (alist-get 'output tokens)))
    (insert (org-opencode--format-message-role role))
    (insert " | ")
    (when timestamp
      (insert (org-opencode--format-session-time timestamp))
      (insert " | "))
    (when model
      (insert (propertize model 'face 'font-lock-string-face))
      (insert " | "))
    (when (or input-tokens output-tokens)
      (insert (format "tokens: %s in / %s out"
                      (or input-tokens "?")
                      (or output-tokens "?")))
      (insert " | "))
    (insert (propertize (org-opencode--format-message-parts-summary parts)
                        'face 'font-lock-doc-face))
    (insert "\n")
    ;; Insert message content preview
    (when parts
      (let ((preview "")
            (max-len 200))
        (catch 'done
          (dolist (part parts)
            (when (equal (alist-get 'type part) "text")
              (let ((text (alist-get 'text part)))
                (when text
                  (setq preview (concat preview text))
                  (when (>= (length preview) max-len)
                    (setq preview (concat (substring preview 0 max-len) "…"))
                    (throw 'done nil)))))))
        (when (> (length preview) 0)
          (insert (propertize (replace-regexp-in-string "\n" " " preview)
                              'face 'shadow))
          (insert "\n"))))
    (insert "\n")))

(defun org-opencode-session-history (&optional session-id)
  "Show message history for SESSION-ID in a buffer.

When SESSION-ID is nil, use the current buffer's session.

Displays all messages with metadata including role, timestamp,
model used, and token counts. For assistant messages, shows a
preview of the response content."
  (interactive)
  (let ((sid (or session-id (org-opencode--session-id))))
    (when (not sid)
      (setq sid (read-string "Session ID: ")))
    (when (string-empty-p sid)
      (user-error "Session ID cannot be empty"))
    (org-opencode--ensure-server)
    (let* ((messages (org-opencode-api-messages sid))
           (session (condition-case nil
                        (org-opencode-api-session sid)
                      (error nil)))
           (buffer (get-buffer-create org-opencode--history-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Session History: %s\n"
                          (or (alist-get 'title session) sid)))
          (insert (make-string 80 ?-))
          (insert "\n\n")
          (if messages
              (dolist (message (reverse messages))
                (org-opencode--insert-message message))
            (insert "No messages found in this session.\n"))
          (goto-char (point-min))
          (forward-line 2)))
      (pop-to-buffer buffer)
      (org-opencode-history-mode))))

(defun org-opencode-sessions-history ()
  "View history for the session at point in the sessions buffer."
  (interactive)
  (let ((session-id (get-text-property (point) 'session-id)))
    (when (not session-id)
      (user-error "No session at point"))
    (org-opencode-session-history session-id)))

;; Session Forking

(defun org-opencode-fork-session (&optional session-id)
  "Fork SESSION-ID into a new session.

When SESSION-ID is nil, use the current buffer's session.
The new session is adopted into the current buffer, replacing
the old session association.

This creates a copy of the session's conversation history that
can diverge from the original."
  (interactive)
  (let ((sid (or session-id (org-opencode--session-id))))
    (when (not sid)
      (setq sid (read-string "Session ID to fork: ")))
    (when (string-empty-p sid)
      (user-error "Session ID cannot be empty"))
    (org-opencode--ensure-server)
    (let ((new-session (org-opencode-api-fork-session sid)))
      (setq org-opencode--session new-session)
      (when (derived-mode-p 'org-mode)
        (org-opencode--remember-session-in-file))
      (message "Forked session %s into new session %s"
               sid
               (org-opencode--session-id)))))

(defun org-opencode-sessions-fork ()
  "Fork the session at point in the sessions buffer.

The new session ID is displayed in the message area. Switch to an
Org buffer and use `org-opencode-adopt-session-by-id' to adopt it."
  (interactive)
  (let ((session-id (get-text-property (point) 'session-id)))
    (when (not session-id)
      (user-error "No session at point"))
    (org-opencode--ensure-server)
    (let ((new-session (org-opencode-api-fork-session session-id)))
      (message "Forked into new session: %s" (alist-get 'id new-session))
      (org-opencode-list-sessions))))

(provide 'org-opencode-session)
;;; org-opencode-session.el ends here
