;;; org-opencode.el --- Org frontend for opencode AI coding agent -*- lexical-binding: t; -*-

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

;; This package provides an Org-mode frontend for opencode, an AI coding agent.
;; It talks to the official stateful `opencode serve` HTTP API and renders
;; streaming responses (including tool calls) directly inside Org buffers.
;;
;; ## Modules
;;
;; The package is split into several modules:
;;
;; - `org-opencode-core`     — HTTP client, server management, API wrappers
;; - `org-opencode-events`   — SSE event stream and handler registry
;; - `org-opencode-session`  — Session CRUD, history, listing
;; - `org-opencode-render`   — Streaming render engine with tool visualization
;; - `org-opencode-approval` — Tool approval UI (permission prompts)
;; - `org-opencode-files`    — File change detection, auto-revert, diff viewer
;; - `org-opencode-ui`       — Header status, model/agent selection, prompt reading
;;
;; ## Quick Start
;;
;;   M-x org-opencode-mode           ; enable in an Org buffer
;;   C-c C-x C-v                     ; send prompt (region or headline)
;;   C-c C-x C-s                     ; new session
;;   C-c C-x C-a                     ; abort current work
;;   C-c C-x C-e                     ; send as structured Org entry
;;   C-c C-x C-r                     ; adopt session from heading

;;; Code:

(require 'org-opencode-core)
(require 'org-opencode-events)
(require 'org-opencode-session)
(require 'org-opencode-render)
(require 'org-opencode-approval)
(require 'org-opencode-files)
(require 'org-opencode-ui)

;; Forward declarations for functions defined in submodules but
;; referenced in the keymap and interactive commands.
(declare-function org-opencode-send-as-entry "org-opencode-ui")

;;; ============================================================
;;; Keymap
;;; ============================================================

(defvar org-opencode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x C-v") #'org-opencode-send)
    (define-key map (kbd "C-c C-x C-s") #'org-opencode-new-session)
    (define-key map (kbd "C-c C-x C-a") #'org-opencode-abort)
    (define-key map (kbd "C-c C-x C-e") #'org-opencode-send-as-entry)
    (define-key map (kbd "C-c C-x C-r") #'org-opencode-adopt-session-from-heading)
    (define-key map (kbd "C-c C-x C-l") #'org-opencode-list-sessions)
    (define-key map (kbd "C-c C-x C-h") #'org-opencode-session-history)
    (define-key map (kbd "C-c C-x C-d") #'org-opencode-show-session-diff)
    (define-key map (kbd "C-c C-x C-m") #'org-opencode-select-model)
    (define-key map (kbd "C-c C-x C-g") #'org-opencode-select-agent)
    (define-key map (kbd "C-c C-x C-f") #'org-opencode-fork-session)
    map)
  "Keymap for `org-opencode-mode'.")

;;; ============================================================
;;; Interactive Commands (top-level)
;;; ============================================================

(defun org-opencode-abort ()
  "Abort any active work in the current session."
  (interactive)
  (unless (org-opencode--session-id)
    (user-error "Current buffer has no opencode session"))
  (org-opencode--ensure-server)
  (org-opencode--http-json
   "POST"
   (org-opencode--path-with-query
    (format "/session/%s/abort" (org-opencode--session-id))
    (org-opencode--session-query)))
  (org-opencode--set-status "Aborted")
  (message "Aborted opencode session %s" (org-opencode--session-id)))

(defun org-opencode-send (prompt)
  "Send PROMPT to the current opencode session and insert the reply.
PROMPT source priority: active region, current headline content,
then minibuffer input.  Use prefix argument to force minibuffer."
  (interactive (list (org-opencode--read-prompt current-prefix-arg)))
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-opencode-send' only works in Org buffers"))
  (when (string-empty-p prompt)
    (user-error "Prompt is empty"))
  (org-opencode--ensure-server)
  (org-opencode--ensure-session)
  (let* ((session-id (org-opencode--session-id))
         (path (org-opencode--path-with-query
                (format "/session/%s/message" session-id)
                (org-opencode--session-query)))
         (payload `((parts . [((type . "text") (text . ,prompt))])
                    ,@(when (bound-and-true-p org-opencode-selected-model)
                        `((providerID . ,(car (split-string org-opencode-selected-model "/")))
                          (modelID . ,(mapconcat #'identity (cdr (split-string org-opencode-selected-model "/")) "/"))))
                    ,@(when (bound-and-true-p org-opencode-selected-agent)
                        `((agentID . ,org-opencode-selected-agent)))))
         (marker nil)
         (state nil))
    (org-opencode-start-event-stream)
    (setq marker (org-opencode--prepare-response-marker prompt session-id))
    (setq state (org-opencode--start-render-state session-id marker))
    (org-opencode--set-status "Streaming")
    (set-marker marker nil)
    (message "Sending prompt to opencode session %s..." session-id)
    (org-opencode--http-json-async
     "POST"
     path
     payload
     (lambda (reply error-message)
       (cond
        (error-message
         (org-opencode--set-error-text state error-message)
         (org-opencode--set-status-in-buffer state (format "Error: %s" error-message))
         (message "opencode request failed: %s" error-message))
        (t
         (org-opencode--merge-final-reply state reply)
         (org-opencode--set-status-in-buffer state "Idle")
         (run-hooks 'org-opencode-after-response-hook)
         (message "Inserted streamed opencode response")))
       (org-opencode--finish-render-state state)))))

;;; ============================================================
;;; Minor Mode
;;; ============================================================

;; We use `org-opencode-after-response-hook' from render module

;;;###autoload
(define-minor-mode org-opencode-mode
  "Minor mode for talking to the opencode backend from Org buffers.

When enabled, this mode:
- Shows a header-line status bar with session and pending state
- Auto-creates or resumes a session tied to the current Org file
- Registers keybindings for sending prompts, switching sessions, etc.

Key bindings:
\\{org-opencode-mode-map}"
  :lighter " OpenCode"
  :keymap org-opencode-mode-map
  (if org-opencode-mode
      (progn
        (setq-local org-opencode--saved-header-line-format header-line-format)
        (org-opencode--apply-header-status-area)
        (org-opencode--set-status "Idle")
        (org-opencode--maybe-auto-session)
        (when org-opencode-show-header-status
          (org-opencode-start-event-stream)))
    ;; Cleanup on disable
    (org-opencode--cleanup-buffer-render-states)
    (setq-local header-line-format org-opencode--saved-header-line-format)
    (kill-local-variable 'org-opencode--saved-header-line-format)
    (kill-local-variable 'org-opencode--status-text)
    (kill-local-variable 'org-opencode--session)
    (kill-local-variable 'org-opencode-selected-model)
    (kill-local-variable 'org-opencode-selected-agent)))

;;; ============================================================
;;; Provide
;;; ============================================================

(provide 'org-opencode)
;;; org-opencode.el ends here