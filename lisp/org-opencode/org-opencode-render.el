;;; org-opencode-render.el --- Rendering engine for org-opencode -*- lexical-binding: t; -*-

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

;; This module implements the streaming render system for org-opencode.
;; It handles real-time visualization of AI responses with rich tool call
;; display using collapsible Org drawers.

;;; Code:

(require 'org-opencode-core)
(require 'org-opencode-events)
(require 'org-opencode-session)
(require 'org)
(require 'subr-x)
(require 'seq)

;; ---------------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------------

(defgroup org-opencode-render nil
  "Rendering and display options for org-opencode."
  :group 'org-opencode
  :prefix "org-opencode-")

(defcustom org-opencode-response-layout 'src-block
  "Layout used when inserting opencode exchanges.

When set to `src-block', insert only a response block at point.
When set to `entry', create a structured Org subtree containing prompt
and response sections."
  :type '(choice (const :tag "Plain src block" src-block)
                 (const :tag "Structured entry" entry))
  :group 'org-opencode-render)

(defcustom org-opencode-store-prompt-in-entry t
  "When non-nil, include the user prompt in structured entry layout."
  :type 'boolean
  :group 'org-opencode-render)

(defcustom org-opencode-entry-heading-prefix "OpenCode"
  "Heading prefix used for structured entry layout."
  :type 'string
  :group 'org-opencode-render)

(defcustom org-opencode-render-nontext-parts t
  "When non-nil, include simple markers for non-text assistant parts."
  :type 'boolean
  :group 'org-opencode-render)

(defcustom org-opencode-after-response-hook nil
  "Hook run after an opencode response is inserted."
  :type 'hook
  :group 'org-opencode-render)

(defcustom org-opencode-show-header-status t
  "When non-nil, show a fixed OpenCode status area in header line."
  :type 'boolean
  :group 'org-opencode-render)

;; `org-opencode-session-id-property' is defined in org-opencode-session.el
;; and available here via (require 'org-opencode-session).

;; ---------------------------------------------------------------------------
;; Global Render State
;; ---------------------------------------------------------------------------

(defvar org-opencode--pending-streams (make-hash-table :test 'equal)
  "Map opencode pending/message keys to active render states.")

(defvar org-opencode--pending-session-queues (make-hash-table :test 'equal)
  "Map opencode session ids to pending render states in send order.")

(defvar org-opencode--pending-sequence 0
  "Sequence counter used to allocate unique pending render keys.")

;; ---------------------------------------------------------------------------
;; Buffer-Local State
;; ---------------------------------------------------------------------------

(defvar-local org-opencode--status-text "Idle"
  "Current OpenCode status text for this Org buffer.")

(defvar-local org-opencode--session-mode nil
  "Current session mode (e.g. \"plan\", \"build\", \"code\"), or nil.")

(defvar-local org-opencode--session-model nil
  "Current model identifier reported by session.status event.")

(defvar-local org-opencode--session-provider nil
  "Current provider identifier reported by session.status event.")

(defvar-local org-opencode--token-input 0
  "Cumulative input token count for the current session.")

(defvar-local org-opencode--token-output 0
  "Cumulative output token count for the current session.")

(defvar-local org-opencode--saved-header-line-format nil
  "Original `header-line-format' saved when org-opencode mode enables.")

;; ---------------------------------------------------------------------------
;; Render State Management
;; ---------------------------------------------------------------------------

(defun org-opencode--make-render-state (session-id marker)
  "Create a render state for SESSION-ID at MARKER."
  (let ((state (make-hash-table :test 'eq)))
    (setq org-opencode--pending-sequence (1+ org-opencode--pending-sequence))
    (puthash :session-id session-id state)
    (puthash :pending-key (format "pending:%s" org-opencode--pending-sequence) state)
    (puthash :buffer (marker-buffer marker) state)
    (puthash :content-start (copy-marker marker nil) state)
    (puthash :content-end nil state)
    (puthash :assistant-message-id nil state)
    (puthash :part-order nil state)
    (puthash :parts (make-hash-table :test 'equal) state)
    state))

(defun org-opencode--pending-state (key)
  "Return the active render state for pending/message KEY, or nil."
  (gethash key org-opencode--pending-streams))

(defun org-opencode--pending-session-states (session-id)
  "Return pending render states for SESSION-ID in send order."
  (gethash session-id org-opencode--pending-session-queues))

(defun org-opencode--set-pending-session-states (session-id states)
  "Set SESSION-ID pending render STATES queue."
  (if states
      (puthash session-id states org-opencode--pending-session-queues)
    (remhash session-id org-opencode--pending-session-queues)))

(defun org-opencode--enqueue-pending-state (session-id state)
  "Append STATE to SESSION-ID pending queue."
  (org-opencode--set-pending-session-states
   session-id
   (append (org-opencode--pending-session-states session-id) (list state))))

(defun org-opencode--next-unbound-state (session-id)
  "Return earliest pending state in SESSION-ID without message id."
  (seq-find (lambda (state)
              (not (gethash :assistant-message-id state)))
            (org-opencode--pending-session-states session-id)))

(defun org-opencode--bind-state-message-id (state message-id)
  "Bind STATE to MESSAGE-ID and update pending indexes."
  (let ((pending-key (gethash :pending-key state)))
    (when pending-key
      (remhash pending-key org-opencode--pending-streams)
      (puthash :pending-key nil state))
    (puthash :assistant-message-id message-id state)
    (puthash message-id state org-opencode--pending-streams))
  state)

(defun org-opencode--state-for-event (session-id message-id)
  "Return state for SESSION-ID and MESSAGE-ID, binding if needed."
  (or (org-opencode--pending-state message-id)
      (let ((state (org-opencode--next-unbound-state session-id)))
        (when state
          (org-opencode--bind-state-message-id state message-id)))))

(defun org-opencode--state-buffer-live-p (state)
  "Return non-nil when STATE still points to a live buffer."
  (buffer-live-p (gethash :buffer state)))

;; ---------------------------------------------------------------------------
;; Part Storage
;; ---------------------------------------------------------------------------

(defun org-opencode--set-part (state part)
  "Store PART in STATE and preserve arrival order."
  (let* ((part-id (alist-get 'id part))
         (parts (gethash :parts state))
         (order (gethash :part-order state)))
    (unless (gethash part-id parts)
      (puthash :part-order (append order (list part-id)) state))
    (puthash part-id part parts)))

;; ---------------------------------------------------------------------------
;; Part Rendering
;; ---------------------------------------------------------------------------

(defun org-opencode--part-text (part)
  "Render PART into display text, or nil if it should be hidden.

Tool calls are rendered as Org drawers for collapsible detail.
Text parts render inline.
Reasoning parts render inline.
Other non-text parts render as type markers when
`org-opencode-render-nontext-parts' is non-nil."
  (pcase (alist-get 'type part)
    ("text" (alist-get 'text part))
    ("reasoning" (alist-get 'text part))
    ("tool" (org-opencode--render-tool-part part))
    ("step-start" nil)
    ("step-finish" nil)
    ((pred (lambda (_type) org-opencode-render-nontext-parts))
     (format "[%s]" (alist-get 'type part)))
    (_ nil)))

(defun org-opencode--render-tool-part (part)
  "Render a tool PART as an Org drawer string.
Collapsed shows summary; expanded shows input/output.

Tool states:
- pending: Tool about to execute
- running: Tool is executing
- completed: Tool finished (show as drawer)
- error: Tool errored"
  (let* ((tool-name (or (alist-get 'tool part) "tool"))
         (state (alist-get 'state part))
         (status (or (alist-get 'status state) "unknown"))
         (input (and state (alist-get 'input state)))
         (output (and state (alist-get 'output state)))
         (error-msg (and state (alist-get 'error state)))
         (title (and state (alist-get 'title state)))
         (icon (pcase tool-name
                 ("Read" "📖") ("Write" "📝") ("Edit" "✏️")
                 ("Bash" "🖥️") ("Grep" "🔍") ("Glob" "📂")
                 ("LSP" "🔮") ("AST" "🌳") ("WebFetch" "🌐")
                 ("Skill" "⚡") (_ "🔧")))
         (drawer-name (format "TOOL_%s" (replace-regexp-in-string "[^A-Za-z0-9_]" "_" tool-name))))
    (pcase status
      ("pending"
       (format "%s %s ▶ Pending..." icon tool-name))
      ("running"
       (format "%s %s ⏳ Running..." icon tool-name))
      ("completed"
       (concat
        (format ":%s:\n" drawer-name)
        (format "%s %s: %s\n" icon tool-name (or title tool-name))
        (when input
          (format "--- Input ---\n%s\n"
                  (org-opencode--format-tool-data input)))
        (when output
          (format "--- Output ---\n%s\n"
                  (org-opencode--format-tool-output output)))
        ":END:"))
      ("error"
       (format "❌ %s: %s" tool-name (or error-msg "unknown error")))
      (_ (format "[%s %s]" tool-name status)))))

(defun org-opencode--format-tool-data (data)
  "Format tool DATA (alist or string) for display."
  (cond
   ((null data) "(none)")
   ((stringp data) data)
   ((listp data)
    (mapconcat (lambda (pair)
                 (cond
                  ((consp pair)
                   (format "%s: %s"
                           (car pair)
                           (if (stringp (cdr pair))
                               (cdr pair)
                             (prin1-to-string (cdr pair)))))
                  (t (prin1-to-string pair))))
               data "\n"))
   (t (prin1-to-string data))))

(defun org-opencode--format-tool-output (output)
  "Format tool OUTPUT string, truncating if very long."
  (if (null output)
      "(no output)"
    (let ((str (if (stringp output)
                   output
                 (prin1-to-string output))))
      (if (> (length str) 2000)
          (concat (substring str 0 2000) "\n... [truncated]")
        str))))

(defun org-opencode--state-render-text (state)
  "Render the current STATE into a markdown string."
  (let ((parts (gethash :parts state)))
    (string-join
     (delq nil
           (mapcar (lambda (part-id)
                     (org-opencode--part-text (gethash part-id parts)))
                   (gethash :part-order state)))
     "\n\n")))

;; ---------------------------------------------------------------------------
;; Incremental Delta Append
;; ---------------------------------------------------------------------------

(defun org-opencode--append-delta (state delta)
  "Append DELTA text incrementally to STATE's content region.
This avoids the full delete-and-rewrite cycle used by
`org-opencode--refresh-render-state' and is suitable for
streaming text deltas where only a small suffix is added."
  (when (and (org-opencode--state-buffer-live-p state) delta)
    (let ((buffer (gethash :buffer state))
          (end (gethash :content-end state)))
      (when (and end (marker-buffer end))
        (with-current-buffer buffer
          (save-excursion
            (let ((inhibit-modification-hooks t)
                  (escaped (org-opencode--escape-src-fences delta)))
              (goto-char end)
              (insert escaped)
              ;; content-end marker has insertion-type nil so it doesn't
              ;; advance — move it past the inserted text manually.
              (set-marker end (point)))))))))

;; ---------------------------------------------------------------------------
;; Buffer Refresh
;; ---------------------------------------------------------------------------

(defun org-opencode--refresh-render-state (state)
  "Rewrite the Org render block for STATE from its accumulated parts."
  (when (org-opencode--state-buffer-live-p state)
    (let ((buffer (gethash :buffer state))
          (start (gethash :content-start state))
          (end (gethash :content-end state)))
      (when (and start end (marker-buffer start) (marker-buffer end))
        (with-current-buffer buffer
          (save-excursion
            (let ((inhibit-modification-hooks t)
                  (text (org-opencode--escape-src-fences
                         (org-opencode--state-render-text state))))
              (goto-char start)
              (delete-region start end)
              (insert text)
              (set-marker end (point)))))))
    ;; Update the buffer's tool drawer visibility
    (org-opencode--update-drawer-visibility (gethash :buffer state))))

(defun org-opencode--update-drawer-visibility (buffer)
  "Update drawer visibility in BUFFER after rendering.

Keeps completed tool drawers collapsed by default."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Look for TOOL_ drawers and ensure they're collapsed
          (while (re-search-forward "^:TOOL_[A-Za-z0-9_]+:$" nil t)
            (let ((drawer-start (match-beginning 0)))
              ;; Try to find the :END: of this drawer
              (when (re-search-forward "^:END:$" nil t)
                ;; Check if drawer is currently expanded
                (when (and (get-char-property drawer-start 'invisible)
                           (not (invisible-p drawer-start)))
                  ;; Drawer is visible (expanded), collapse it
                  (goto-char drawer-start)
                  (org-cycle))))))))))

;; ---------------------------------------------------------------------------
;; Response Layout Helpers
;; ---------------------------------------------------------------------------

(defun org-opencode--create-response-block ()
  "Insert an empty markdown src block and return its content markers."
  (let (start end)
    (unless (bolp)
      (insert "\n"))
    (insert "#+begin_src markdown\n")
    (setq start (copy-marker (point) nil))
    (setq end (copy-marker (point) nil))
    (insert "\n#+end_src\n")
    (list start end)))

(defun org-opencode--entry-stars (&optional extra-level)
  "Return Org stars for current heading level plus EXTRA-LEVEL.

At top-level (outside any heading), this returns one star."
  (let* ((base-level (or (org-current-level) 0))
         (level (+ base-level (or extra-level 1))))
    (make-string (max 1 level) ?*)))

(defun org-opencode--entry-title ()
  "Return the default title for a structured opencode entry."
  (format "%s %s"
          org-opencode-entry-heading-prefix
          (format-time-string "%Y-%m-%d %H:%M")))

(defun org-opencode--insert-text-src-block (text)
  "Insert TEXT as a plain text source block."
  (insert "#+begin_src text\n")
  (insert (org-opencode--escape-src-fences text))
  (unless (bolp)
    (insert "\n"))
  (insert "#+end_src\n"))

(defun org-opencode--insert-exchange-shell (prompt session-id)
  "Insert a structured entry for PROMPT and SESSION-ID.

Return a marker that points to where the streamed response should be
inserted."
  (let* ((entry-stars (org-opencode--entry-stars 1))
         (child-stars (org-opencode--entry-stars 2))
         (response-marker nil))
    (unless (bolp)
      (insert "\n"))
    (insert (format "%s %s\n" entry-stars (org-opencode--entry-title)))
    (insert ":PROPERTIES:\n")
    (insert (format ":%s: %s\n" org-opencode-session-id-property session-id))
    (insert ":END:\n")
    (when org-opencode-store-prompt-in-entry
      (insert (format "%s Prompt\n" child-stars))
      (org-opencode--insert-text-src-block prompt)
      (insert "\n"))
    (insert (format "%s Response\n" child-stars))
    (setq response-marker (copy-marker (point) t))
    response-marker))

;; ---------------------------------------------------------------------------
;; Render State Lifecycle
;; ---------------------------------------------------------------------------

(defun org-opencode--start-render-state (session-id marker)
  "Create and register a streaming render state for SESSION-ID at MARKER."
  (let* ((state (org-opencode--make-render-state session-id marker))
         (positions nil))
    (when (org-opencode--state-buffer-live-p state)
      (with-current-buffer (gethash :buffer state)
        (save-excursion
          (goto-char marker)
          (setq positions (org-opencode--create-response-block)))))
    (set-marker (gethash :content-start state) (car positions))
    (puthash :content-end (cadr positions) state)
    (puthash (gethash :pending-key state) state org-opencode--pending-streams)
    (org-opencode--enqueue-pending-state session-id state)
    state))

(defun org-opencode--finish-render-state (state)
  "Forget STATE from all pending indexes and release markers."
  (when state
    (let* ((session-id (gethash :session-id state))
           (message-id (gethash :assistant-message-id state))
           (pending-key (gethash :pending-key state))
           (start (gethash :content-start state))
           (end (gethash :content-end state))
           (remaining (delq state (copy-sequence (org-opencode--pending-session-states session-id)))))
      (org-opencode--set-pending-session-states session-id remaining)
      (when message-id
        (remhash message-id org-opencode--pending-streams))
      (when pending-key
        (remhash pending-key org-opencode--pending-streams))
      (when start
        (set-marker start nil))
      (when end
        (set-marker end nil)))))

(defun org-opencode--cleanup-buffer-render-states (&optional buffer)
  "Finish all pending render states that belong to BUFFER.
BUFFER defaults to the current buffer.  This is intended to be
called when `org-opencode-mode' is disabled so that dangling
markers and hash-table entries are released."
  (let ((buf (or buffer (current-buffer))))
    (maphash
     (lambda (_key state)
       (when (eq (gethash :buffer state) buf)
         (org-opencode--finish-render-state state)))
     ;; Iterate a copy because finish mutates the table.
     (copy-hash-table org-opencode--pending-streams))))

(defun org-opencode--set-error-text (state error-message)
  "Render ERROR-MESSAGE into STATE."
  (clrhash (gethash :parts state))
  (puthash :part-order '("error") state)
  (puthash "error"
           `((id . "error")
             (type . "text")
             (text . ,(format "opencode request failed: %s" error-message)))
           (gethash :parts state))
  (org-opencode--refresh-render-state state))

(defun org-opencode--merge-final-reply (state reply)
  "Merge final assistant REPLY into STATE and refresh the buffer."
  (let ((info (alist-get 'info reply))
        (parts (alist-get 'parts reply)))
    (when info
      (puthash :assistant-message-id (alist-get 'id info) state))
    (when parts
      (dolist (part parts)
        (org-opencode--set-part state part)))
    (org-opencode--refresh-render-state state)))

;; ---------------------------------------------------------------------------
;; Text Utilities
;; ---------------------------------------------------------------------------

(defun org-opencode--escape-src-fences (text)
  "Escape Org src block delimiters in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward
            "^[[:space:]]*#\\+\\(begin\\|end\\)_src\\b" nil t)
      (replace-match ",\\&" t))
    (buffer-string)))

;; ---------------------------------------------------------------------------
;; Status Management
;; ---------------------------------------------------------------------------

(defun org-opencode--pending-count (&optional session-id)
  "Return pending state count for SESSION-ID or current session."
  (length (org-opencode--pending-session-states
           (or session-id (org-opencode--session-id)))))

(defun org-opencode--set-status (text)
  "Set current buffer OpenCode status to TEXT."
  (setq org-opencode--status-text text)
  (force-mode-line-update t))

(defun org-opencode--set-status-in-buffer (state text)
  "Set OpenCode status to TEXT in the buffer associated with render STATE.
This is safe to call from async event handlers where `current-buffer'
may not be the target Org buffer."
  (let ((buffer (gethash :buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (org-opencode--set-status text)))))

(defun org-opencode--header-status-text ()
  "Return one-line status text for fixed top display."
  (let* ((session (or (org-opencode--session-id) "none"))
         ;; Prefer live model from session.status; fallback to user selection.
         (model (or org-opencode--session-model
                    (bound-and-true-p org-opencode-selected-model)
                    "default"))
         (provider (or org-opencode--session-provider ""))
         (agent (or (bound-and-true-p org-opencode-selected-agent) "default"))
         (mode (or org-opencode--session-mode ""))
         (tokens (if (or (> org-opencode--token-input 0)
                         (> org-opencode--token-output 0))
                     (format " | tokens:%d/%d"
                             org-opencode--token-input
                             org-opencode--token-output)
                   ""))
         ;; Build mode segment: "[plan]" or "[build]" etc.
         (mode-str (if (and mode (not (string-empty-p mode)))
                       (format "[%s]" mode)
                     ""))
         ;; Build provider/model segment
         (model-str (if (and provider (not (string-empty-p provider)))
                        (format "%s/%s" provider model)
                      model)))
    (format " OpenCode %s | %s | %s | %s | session:%s | pending:%d%s "
            mode-str
            org-opencode--status-text
            model-str
            agent
            (org-opencode--truncate-session-id-for-header session)
            (org-opencode--pending-count)
            tokens)))

(defun org-opencode--truncate-session-id-for-header (session-id)
  "Truncate SESSION-ID for header-line display (max 8 chars)."
  (if (and session-id (> (length session-id) 8))
      (substring session-id 0 8)
    (or session-id "none")))

(defun org-opencode--apply-header-status-area ()
  "Install or remove fixed header status area based on user option."
  (if org-opencode-show-header-status
      (setq-local header-line-format '(:eval (org-opencode--header-status-text)))
    (setq-local header-line-format org-opencode--saved-header-line-format)))

(defun org-opencode--fetch-initial-session-status ()
  "Fetch mode/model/provider from the last assistant message.
Called during mode enable to show current model/mode immediately.
The /session/status API does not include mode/model/provider fields,
so we retrieve them from the most recent assistant message's info."
  (condition-case nil
      (let* ((session-id (org-opencode--session-id))
             (messages (and session-id (org-opencode-api-messages session-id))))
        (when messages
          ;; Walk messages in reverse to find the last assistant message
          (catch 'done
            (dolist (msg (reverse messages))
              (let ((role (alist-get 'role msg)))
                (when (and role (equal role "assistant"))
                  (let* ((info (alist-get 'info msg))
                         (mode (and info (alist-get 'mode info)))
                         (model-id (and info (alist-get 'modelID info)))
                         (provider-id (and info (alist-get 'providerID info))))
                    (when mode (setq org-opencode--session-mode mode))
                    (when model-id (setq org-opencode--session-model model-id))
                    (when provider-id (setq org-opencode--session-provider provider-id))
                    (force-mode-line-update t)
                    (throw 'done nil))))))
          ;; Still update idle/busy status from session-status API
          (condition-case nil
              (let* ((all-status (and session-id (org-opencode-api-session-status)))
                     (status (and all-status (alist-get (intern session-id) all-status))))
                (when status
                  (let ((state (alist-get 'status status)))
                    (when state
                      (setq org-opencode--status-text
                            (if (equal state "busy") "busy" "idle"))
                      (force-mode-line-update t)))))
            (error nil))))
    (error nil)))

;; ---------------------------------------------------------------------------
;; Legacy/Compatibility Functions
;; ---------------------------------------------------------------------------

(defun org-opencode--insert-response (marker output)
  "Insert OUTPUT at MARKER as a markdown src block.
This is a legacy function for non-streaming insertion."
  (when (marker-buffer marker)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (unless (bolp)
          (insert "\n"))
        (insert "#+begin_src markdown\n")
        (insert (org-opencode--escape-src-fences output))
        (unless (bolp)
          (insert "\n"))
        (insert "#+end_src\n"))
      (run-hooks 'org-opencode-after-response-hook))))

(defun org-opencode--parts-text (parts)
  "Render PARTS into markdown text for Org insertion.
This is a legacy function for non-streaming rendering."
  (string-join
   (delq
    nil
    (mapcar
     (lambda (part)
       (pcase (alist-get 'type part)
         ("text" (alist-get 'text part))
         ("reasoning" (alist-get 'text part))
         ("tool" (org-opencode--render-tool-part part))
         ((or "step-start" "step-finish") nil)
         ((pred (lambda (_type) org-opencode-render-nontext-parts))
          (format "[%s]" (alist-get 'type part)))
         (_ nil)))
     parts))
   "\n\n"))

(defun org-opencode--prepare-response-marker (prompt session-id)
  "Return an insertion marker for PROMPT in SESSION-ID.

Marker placement depends on `org-opencode-response-layout'."
  (pcase org-opencode-response-layout
    ('entry
     (org-opencode--insert-exchange-shell prompt session-id))
    (_
     (copy-marker (point) t))))

;; ---------------------------------------------------------------------------
;; Event Handlers
;; ---------------------------------------------------------------------------

(defun org-opencode--event-message-updated (session-id properties)
  "Handle `message.updated` event for SESSION-ID with PROPERTIES.
Only processes assistant messages.  User messages are ignored to prevent
accidentally binding a render state to the wrong message ID (since
`org-opencode--state-for-event' has the side effect of binding unbound
states to the given message ID)."
  (let* ((info (alist-get 'info properties))
         (role (alist-get 'role info))
         (message-id (alist-get 'id info)))
    ;; Guard: only bind render state for assistant messages.
    ;; Calling `org-opencode--state-for-event' for user messages would
    ;; consume the unbound pending state, leaving no state available
    ;; when the assistant message arrives — causing all streaming
    ;; deltas to be silently dropped.
    (when (and message-id (equal role "assistant"))
      (let ((state (org-opencode--state-for-event session-id message-id)))
        (when state
          (puthash :assistant-message-id message-id state)
          (let ((buffer (gethash :buffer state))
                (mode (alist-get 'mode info))
                (model-id (alist-get 'modelID info))
                (provider-id (alist-get 'providerID info))
                (usage (alist-get 'usage info)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when mode
                  (setq org-opencode--session-mode mode))
                (when model-id
                  (setq org-opencode--session-model model-id))
                (when provider-id
                  (setq org-opencode--session-provider provider-id))
                (when usage
                  (setq org-opencode--token-input
                        (or (alist-get 'input usage) org-opencode--token-input))
                  (setq org-opencode--token-output
                        (or (alist-get 'output usage) org-opencode--token-output)))
                (force-mode-line-update t)))))))))

(defun org-opencode--event-part-updated (_session-id properties)
  "Handle `message.part.updated` event with PROPERTIES.
_SESSION-ID is provided by the event dispatch but not needed here
since part carries its own sessionID.

Uses `org-opencode--pending-state' for a direct lookup instead of
`org-opencode--state-for-event' to avoid the binding side effect.
User message parts arrive before the assistant message.updated event,
and calling state-for-event here would bind the render state to the
user message ID — leaving no state for the real assistant message."
  (let* ((part (alist-get 'part properties))
         (_session-id (alist-get 'sessionID part))
         (message-id (alist-get 'messageID part))
         (state (and message-id
                     (org-opencode--pending-state message-id))))
    (when (and state
               (gethash :assistant-message-id state)
               (equal (gethash :assistant-message-id state) message-id))
      (when (equal (alist-get 'type part) "tool")
        (let* ((tool-name (or (alist-get 'tool part) "tool"))
               (tool-state (alist-get 'state part))
               (tool-status (or (alist-get 'status tool-state) "unknown")))
          (org-opencode--set-status-in-buffer state (format "Tool %s: %s" tool-name tool-status))))
      (org-opencode--set-part state part)
      (org-opencode--refresh-render-state state))))

(defun org-opencode--event-part-delta (session-id properties)
  "Handle `message.part.delta` event for SESSION-ID with PROPERTIES.

Uses `org-opencode--pending-state' for a direct lookup instead of
`org-opencode--state-for-event' to avoid the binding side effect.
See `org-opencode--event-part-updated' for rationale."
  (let* ((message-id (alist-get 'messageID properties))
         (state (and message-id
                     (org-opencode--pending-state message-id)))
         (part-id (alist-get 'partID properties))
         (field (alist-get 'field properties))
         (delta (alist-get 'delta properties)))
    (when (and state
               (equal field "text")
               (gethash :assistant-message-id state)
               (equal (gethash :assistant-message-id state) message-id))
      (org-opencode--set-status-in-buffer state "Streaming")
      (let* ((parts (gethash :parts state))
             (part (or (gethash part-id parts)
                       `((id . ,part-id)
                         (sessionID . ,session-id)
                         (messageID . ,message-id)
                         (type . "text")
                         (text . ""))))
             (text (concat (or (alist-get 'text part) "") delta))
             (updated (cons `(text . ,text)
                            (assq-delete-all 'text part))))
        (org-opencode--set-part state updated)
        (org-opencode--append-delta state delta)))))

(defun org-opencode--event-session-idle (session-id _properties)
  "Handle `session.idle` event for SESSION-ID.
Finishes all pending render states for this session (streaming is done)
and sets status to Idle."
  (let ((states (copy-sequence (org-opencode--pending-session-states session-id))))
    (if states
        (dolist (state states)
          ;; Do a final full refresh from accumulated parts.
          (org-opencode--refresh-render-state state)
          (org-opencode--set-status-in-buffer state "Idle")
          (let ((buffer (gethash :buffer state)))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (run-hooks 'org-opencode-after-response-hook))))
          (org-opencode--finish-render-state state)
          (message "Inserted streamed opencode response"))
      ;; No pending states — try to update any buffer whose session matches.
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (bound-and-true-p org-opencode-mode)
                       (equal (org-opencode--session-id) session-id))
              (org-opencode--set-status "Idle"))))))))

(defun org-opencode--event-session-error (session-id properties)
  "Handle `session.error` event for SESSION-ID with PROPERTIES.
Sets error status in all buffers that have pending render states
for this session, and writes the error into the latest pending
render block."
  (let* ((error-info (alist-get 'error properties))
         (error-text (cond
                      ((stringp error-info) error-info)
                      ((listp error-info) (or (alist-get 'message error-info)
                                              (format "%S" error-info)))
                      (t (format "%S" error-info))))
         (states (org-opencode--pending-session-states session-id)))
    (if states
        (dolist (state states)
          (org-opencode--set-status-in-buffer state (format "Error: %s" error-text))
          ;; Write error into the most recent pending render block
          (when (org-opencode--state-buffer-live-p state)
            (org-opencode--set-error-text state error-text)))
      ;; Fallback: update matching buffers directly
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (bound-and-true-p org-opencode-mode)
                       (equal (org-opencode--session-id) session-id))
              (org-opencode--set-status (format "Error: %s" error-text)))))))))

(defun org-opencode--event-todo-updated (session-id properties)
  "Handle `todo.updated` event for SESSION-ID with PROPERTIES.
Updates the status line with a summary of agent todo progress."
  (let* ((todos (alist-get 'todos properties))
         (total (length todos))
         (completed (length (seq-filter
                             (lambda (t-item)
                               (equal (alist-get 'status t-item) "completed"))
                             todos)))
         (in-progress (seq-find
                       (lambda (t-item)
                         (equal (alist-get 'status t-item) "in_progress"))
                       todos))
         (status-text (if in-progress
                          (format "Todo %d/%d: %s"
                                  completed total
                                  (or (alist-get 'content in-progress) "working..."))
                        (format "Todo %d/%d" completed total))))
    ;; Route to pending state buffers first, then fallback to session buffers
    (let ((states (org-opencode--pending-session-states session-id)))
      (if states
          (dolist (state states)
            (org-opencode--set-status-in-buffer state status-text))
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (and (bound-and-true-p org-opencode-mode)
                         (equal (org-opencode--session-id) session-id))
                (org-opencode--set-status status-text)))))))))

(defun org-opencode--event-session-status (session-id properties)
  "Handle `session.status` event for SESSION-ID with PROPERTIES.
Updates mode (plan/build/code), model, and provider in matching buffers."
  (let* ((status (alist-get 'status properties))
         (mode (alist-get 'mode status))
         (model (alist-get 'model status))
         (provider (alist-get 'provider status))
         (model-id (alist-get 'modelID status)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (bound-and-true-p org-opencode-mode)
                     (equal (org-opencode--session-id) session-id))
            (when mode
              (setq org-opencode--session-mode mode))
            (when (or model model-id)
              (setq org-opencode--session-model (or model model-id)))
            (when provider
              (setq org-opencode--session-provider provider))
            (force-mode-line-update t)))))))

;; ---------------------------------------------------------------------------
;; Register Event Handlers
;; ---------------------------------------------------------------------------

(org-opencode-register-event-handler "message.updated" #'org-opencode--event-message-updated)
(org-opencode-register-event-handler "message.part.updated" #'org-opencode--event-part-updated)
(org-opencode-register-event-handler "message.part.delta" #'org-opencode--event-part-delta)
(org-opencode-register-event-handler "session.idle" #'org-opencode--event-session-idle)
(org-opencode-register-event-handler "session.error" #'org-opencode--event-session-error)
(org-opencode-register-event-handler "session.status" #'org-opencode--event-session-status)
(org-opencode-register-event-handler "todo.updated" #'org-opencode--event-todo-updated)

(provide 'org-opencode-render)
;;; org-opencode-render.el ends here
