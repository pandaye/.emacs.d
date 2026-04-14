;;; org-opencode-events.el --- SSE event stream for org-opencode -*- lexical-binding: t; -*-

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

;; This module provides Server-Sent Events (SSE) streaming support for
;; org-opencode. It manages the HTTP event stream connection to the
;; opencode server and dispatches incoming events to registered handlers.

;; Handler Registry Pattern:
;;
;; Instead of hardcoding event handlers, this module implements a plugin-style
;; registry system. Other modules (render, approval, files, etc.) register
;; their handlers at load time using `org-opencode-register-event-handler'.
;;
;; Example:
;;   (org-opencode-register-event-handler
;;    "message.part.delta"
;;    (lambda (session-id properties)
;;      ;; Handle text delta updates
;;      ))
;;
;; Handlers are called with (SESSION-ID PROPERTIES) where:
;;   - SESSION-ID is the opencode session identifier
;;   - PROPERTIES is the event-specific data as an alist
;;
;; Handlers run asynchronously via `run-with-timer' to avoid blocking the
;; process filter. The module itself does not depend on rendering logic -
;; it only parses and dispatches events.

;;; Code:

(require 'org-opencode-core)

;; ============================================================================
;; Event Handler Registry
;; ============================================================================

(defvar org-opencode-event-handlers nil
  "Alist of (EVENT-TYPE . HANDLER-LIST) for SSE event dispatch.
Each HANDLER-LIST is a list of functions that receive (SESSION-ID PROPERTIES)
and run in the context of the relevant org buffer when possible.")

(defun org-opencode-register-event-handler (event-type handler)
  "Register HANDLER for EVENT-TYPE.
HANDLER is called with (SESSION-ID PROPERTIES).
Multiple handlers can be registered for the same event type."
  (let ((existing (assoc event-type org-opencode-event-handlers)))
    (if existing
        (unless (memq handler (cdr existing))
          (setcdr existing (cons handler (cdr existing))))
      (push (cons event-type (list handler)) org-opencode-event-handlers))))

(defun org-opencode-unregister-event-handler (event-type handler)
  "Remove HANDLER from EVENT-TYPE handlers.
Returns non-nil if handler was found and removed."
  (let ((existing (assoc event-type org-opencode-event-handlers)))
    (when existing
      (setcdr existing (delq handler (cdr existing)))
      (unless (cdr existing)
        (setq org-opencode-event-handlers
              (delq existing org-opencode-event-handlers)))
      t)))

(defun org-opencode-get-event-handlers (event-type)
  "Return list of handlers registered for EVENT-TYPE."
  (cdr (assoc event-type org-opencode-event-handlers)))

;; ============================================================================
;; SSE Stream Management
;; ============================================================================

(defvar org-opencode--event-process nil
  "Process object for the shared opencode SSE event stream.
There is at most one event stream process shared across all sessions.")

(defvar org-opencode--event-reconnect-timer nil
  "Timer for SSE auto-reconnect, or nil when no reconnect is pending.")

(defvar org-opencode--event-reconnect-attempts 0
  "Number of consecutive reconnect attempts since last successful connection.")

(defconst org-opencode--event-max-reconnect-delay 60
  "Maximum delay in seconds between SSE reconnect attempts.")

(defconst org-opencode--event-buffer-name "*org-opencode-events*"
  "Buffer used for opencode SSE event logs and debugging.
Contains raw SSE traffic for troubleshooting.")

(defun org-opencode--event-process-live-p ()
  "Return non-nil when the shared SSE event process is alive."
  (and org-opencode--event-process
       (process-live-p org-opencode--event-process)))

(defun org-opencode--event-url ()
  "Return the SSE endpoint URL."
  (concat (org-opencode--base-url) "/event"))

(defun org-opencode-start-event-stream ()
  "Start the shared `/event` subscription if not already running.
Ensures the opencode server is healthy before starting the stream.
Idempotent - safe to call multiple times."
  (interactive)
  (org-opencode--ensure-server)
  (unless (org-opencode--event-process-live-p)
    (let ((buffer (get-buffer-create org-opencode--event-buffer-name)))
      (setq org-opencode--event-process
            (make-process
             :name "org-opencode-events"
             :buffer buffer
             :command (list (org-opencode--curl-executable)
                            "-NsS"
                            (org-opencode--event-url))
             :coding 'utf-8
             :connection-type 'pipe
             :filter #'org-opencode--event-filter
             :sentinel #'org-opencode--event-sentinel
             :noquery t))
      ;; Initialize process state for SSE parsing
      (process-put org-opencode--event-process :remainder "")
      (process-put org-opencode--event-process :data-lines nil)
      (message "Subscribed to opencode /event stream"))))

(defun org-opencode-stop-event-stream ()
  "Stop the shared `/event` subscription.
Signals graceful shutdown to suppress unexpected exit warnings.
Cancels any pending reconnect timer."
  (interactive)
  (org-opencode--cancel-reconnect-timer)
  (setq org-opencode--event-reconnect-attempts 0)
  (unless (org-opencode--event-process-live-p)
    (user-error "No opencode event stream is active"))
  (process-put org-opencode--event-process :stopping t)
  (delete-process org-opencode--event-process)
  (setq org-opencode--event-process nil)
  (message "Stopped opencode /event stream"))

;; ============================================================================
;; SSE Protocol Parsing
;; ============================================================================

(defun org-opencode--event-sentinel (process _event)
  "Handle lifecycle changes for the SSE PROCESS.
Detects unexpected exits and schedules auto-reconnect with exponential
backoff.  Graceful shutdowns (marked with :stopping) are silently accepted."
  (unless (process-live-p process)
    (when (eq process org-opencode--event-process)
      (setq org-opencode--event-process nil))
    (cond
     ;; Graceful stop — no reconnect.
     ((or (process-get process :stopping)
          (memq (process-exit-status process) '(0 15)))
      nil)
     ;; Unexpected exit — schedule reconnect.
     (t
      (let* ((attempt org-opencode--event-reconnect-attempts)
             (delay (min (expt 2 attempt) org-opencode--event-max-reconnect-delay)))
        (setq org-opencode--event-reconnect-attempts (1+ attempt))
        (message "org-opencode event stream exited unexpectedly, reconnecting in %ds..." delay)
        (org-opencode--cancel-reconnect-timer)
        (setq org-opencode--event-reconnect-timer
              (run-with-timer delay nil #'org-opencode--try-reconnect)))))))

(defun org-opencode--cancel-reconnect-timer ()
  "Cancel any pending SSE reconnect timer."
  (when org-opencode--event-reconnect-timer
    (cancel-timer org-opencode--event-reconnect-timer)
    (setq org-opencode--event-reconnect-timer nil)))

(defun org-opencode--try-reconnect ()
  "Attempt to reconnect the SSE event stream.
Resets reconnect state on success.  If the server is not reachable,
the next attempt will be scheduled by the sentinel."
  (setq org-opencode--event-reconnect-timer nil)
  (condition-case err
      (progn
        (org-opencode-start-event-stream)
        ;; If we get here the process started — reset backoff.
        (setq org-opencode--event-reconnect-attempts 0)
        (message "org-opencode event stream reconnected"))
    (error
     (message "org-opencode reconnect failed: %s" (error-message-string err))
     ;; Sentinel will schedule the next attempt when the process exits.
     )))

(defun org-opencode--event-filter (process chunk)
  "Handle SSE CHUNK from PROCESS.
Accumulates partial lines and dispatches complete lines for processing.
Uses :remainder process property to store incomplete line data.
Respects SSE line format with optional \\r\\n line endings."
  (let ((input (concat (or (process-get process :remainder) "") chunk))
        line)
    (while (string-match "\n" input)
      (setq line (substring input 0 (match-beginning 0))
            input (substring input (match-end 0)))
      (org-opencode--event-line process (string-remove-suffix "\r" line)))
    (process-put process :remainder input)))

(defun org-opencode--event-line (process line)
  "Handle one SSE LINE from PROCESS.
Accumulates \\='data:\\=' lines until an empty line marks the end of an event,
then joins all data lines and dispatches the complete event payload."
  (if (string-empty-p line)
      ;; Empty line signals end of event - dispatch accumulated data
      (let ((data-lines (process-get process :data-lines)))
        (when data-lines
          (process-put process :data-lines nil)
          (org-opencode--dispatch-event
           (string-join (nreverse data-lines) "\n"))))
    ;; Accumulate data lines
    (when (string-prefix-p "data:" line)
      (process-put process :data-lines
                   (cons (string-trim-left (substring line 5))
                         (process-get process :data-lines))))))

;; ============================================================================
;; Event Dispatch
;; ============================================================================

(defun org-opencode--dispatch-event (payload)
  "Dispatch one SSE PAYLOAD to registered handlers.
Parses the JSON payload, extracts event type and properties,
then calls all registered handlers for that event type.
Handlers are run asynchronously to avoid blocking.
All event types from the opencode API are supported:
  - message.updated, message.removed
  - message.part.updated, message.part.delta, message.part.removed
  - permission.updated, permission.replied
  - session.status, session.idle, session.compacted
  - session.created, session.updated, session.deleted, session.diff
  - session.error
  - file.edited, file.watcher.updated
  - todo.updated
  - command.executed
  - lsp.client.diagnostics, lsp.updated
  - server.connected, server.instance.disposed
  - installation.updated, installation.update-available"
  (condition-case err
      (let* ((event (json-parse-string payload
                                      :object-type 'alist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object :json-false))
             (type (alist-get 'type event))
             (properties (alist-get 'properties event))
             (session-id (org-opencode--extract-session-id type properties))
             (handlers (org-opencode-get-event-handlers type)))
        ;; Call all registered handlers for this event type
        (dolist (handler handlers)
          ;; Run handler asynchronously to avoid blocking process filter
          (run-with-timer 0 nil handler session-id properties))
        ;; Log unhandled event types in debug mode
        (unless (or handlers (string-prefix-p "session." type))
          (org-opencode--log-debug "No handlers for event type: %s" type)))
    (error
     (message "Failed to parse opencode event: %s" (error-message-string err)))))

(defun org-opencode--extract-session-id (event-type properties)
  "Extract session ID from PROPERTIES based on EVENT-TYPE.
Returns nil if session ID cannot be determined."
  (cond
   ;; Direct sessionID property
   ((alist-get 'sessionID properties))
   ;; Nested in info object
   ((let ((info (alist-get 'info properties)))
      (when info (alist-get 'sessionID info))))
   ;; Nested in part object (for message.part.* events)
   ((let ((part (alist-get 'part properties)))
      (when part (alist-get 'sessionID part))))
   ;; Server events may not have session
   ((member event-type '("server.connected" "server.instance.disposed")) nil)
   ;; Installation events don't have session
   ((member event-type '("installation.updated" "installation.update-available")) nil)
   ;; Default: try sessionID directly
   (t (alist-get 'sessionID properties))))

;; ============================================================================
;; Convenience: Batch Handler Registration
;; ============================================================================

(defun org-opencode-register-handlers (handler-alist)
  "Register multiple handlers from HANDLER-ALIST.
HANDLER-ALIST is a list of (EVENT-TYPE . HANDLER) pairs.
Returns the number of handlers registered."
  (let ((count 0))
    (dolist (pair handler-alist)
      (org-opencode-register-event-handler (car pair) (cdr pair))
      (setq count (1+ count)))
    count))

(defun org-opencode-clear-event-handlers (&optional event-type)
  "Clear all handlers for EVENT-TYPE, or all handlers if EVENT-TYPE is nil.
Useful for testing or resetting the handler registry."
  (if event-type
      (setq org-opencode-event-handlers
            (delq (assoc event-type org-opencode-event-handlers)
                  org-opencode-event-handlers))
    (setq org-opencode-event-handlers nil)))

;; ============================================================================
;; Debug Utilities
;; ============================================================================

(defun org-opencode--log-debug (format-string &rest args)
  "Log debug message to event buffer if it exists.
FORMAT-STRING and ARGS are passed to `format'."
  (when (get-buffer org-opencode--event-buffer-name)
    (with-current-buffer (get-buffer-create org-opencode--event-buffer-name)
      (goto-char (point-max))
      (insert (format "[DEBUG %s] "
                      (format-time-string "%H:%M:%S")))
      (insert (apply #'format format-string args))
      (insert "\n"))))

(defun org-opencode-list-event-handlers ()
  "Display all registered event handlers in a help buffer.
Useful for debugging handler registration."
  (interactive)
  (with-help-window "*org-opencode event handlers*"
    (princ "Registered OpenCode Event Handlers\n")
    (princ "==================================\n\n")
    (if org-opencode-event-handlers
        (dolist (entry org-opencode-event-handlers)
          (princ (format "Event: %s\n" (car entry)))
          (dolist (handler (cdr entry))
            (princ (format "  - %s\n" handler)))
          (princ "\n"))
      (princ "No handlers registered.\n"))))

;; ============================================================================
;; Event Type Documentation
;; ============================================================================

;; The following event types are dispatched from the opencode API:
;;
;; Message Events:
;;   message.updated      - properties has `info' with message fields
;;   message.removed      - properties has `sessionID', `messageID'
;;   message.part.updated - properties has `part' with full part data
;;   message.part.delta   - properties has `sessionID', `messageID',
;;                          `partID', `field', `delta'
;;   message.part.removed - properties has `sessionID', `messageID', `partID'
;;
;; Permission Events:
;;   permission.updated   - properties has permission object with
;;                          `id', `sessionID', `type', `title', `metadata'
;;   permission.replied   - properties has `sessionID', `permissionID', `response'
;;
;; Session Events:
;;   session.status       - properties has `sessionID', `status' object
;;   session.idle         - properties has `sessionID'
;;   session.compacted    - properties has `sessionID'
;;   session.created      - properties has `info' (full session)
;;   session.updated      - properties has `info' (full session)
;;   session.deleted      - properties has `info' (full session)
;;   session.diff         - properties has `sessionID', `diff' (FileDiff array)
;;   session.error        - properties has `sessionID' and `error'
;;
;; File Events:
;;   file.edited          - properties has `file' (filepath string)
;;   file.watcher.updated - properties has `file', `event' ("add"|"change"|"unlink")
;;
;; Todo Events:
;;   todo.updated         - properties has `sessionID', `todos' array
;;
;; Command Events:
;;   command.executed     - properties has `name', `sessionID', `arguments',
;;                          `messageID'
;;
;; LSP Events:
;;   lsp.client.diagnostics - properties has `serverID', `path'
;;   lsp.updated            - properties has update data
;;
;; Server Events:
;;   server.connected         - properties has server info
;;   server.instance.disposed - properties has `directory'
;;
;; Installation Events:
;;   installation.updated       - properties has `version'
;;   installation.update-available - properties has `version'

(provide 'org-opencode-events)
;;; org-opencode-events.el ends here
