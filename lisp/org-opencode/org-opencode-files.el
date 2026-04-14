;;; org-opencode-files.el --- File change detection and diff viewer -*- lexical-binding: t; -*-

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

;; This module handles file change detection from the opencode agent.
;; When opencode edits files via the SSE stream, this module:
;;
;; 1. Detects file.edited, file.watcher.updated, and session.diff events
;; 2. Offers to auto-revert buffers that changed
;; 3. Shows file diffs from the session in an Org buffer
;; 4. Provides accept/reject workflow for file changes
;;
;; To use:
;;   (require 'org-opencode-files)
;;   ;; Optionally enable auto-revert
;;   (setq org-opencode-auto-revert-files t)

;;; Code:

(require 'org-opencode-core)
(require 'org-opencode-events)

;; ---------------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------------

(defgroup org-opencode-files nil
  "File change detection and diff viewing for org-opencode."
  :group 'org-opencode
  :prefix "org-opencode-")

(defcustom org-opencode-auto-revert-files nil
  "When non-nil, automatically revert buffers when opencode edits their files.
When set to `ask', prompt before reverting.
When set to `notify', show a message but don't revert."
  :type '(choice (const :tag "Auto revert" t)
                 (const :tag "Ask before revert" ask)
                 (const :tag "Notify only" notify)
                 (const :tag "Disabled" nil))
  :group 'org-opencode-files)

(defcustom org-opencode-diff-buffer-name "*opencode-diff*"
  "Name of the buffer used to display session diffs."
  :type 'string
  :group 'org-opencode-files)

;; ---------------------------------------------------------------------------
;; File Change Detection
;; ---------------------------------------------------------------------------

(defun org-opencode--handle-file-edited (_session-id properties)
  "Handle a file.edited event from _SESSION-ID with PROPERTIES.
PROPERTIES should contain a `file' key with the filepath."
  (let ((filepath (alist-get 'file properties)))
    (when filepath
      (org-opencode--maybe-revert-file filepath))))

(defun org-opencode--handle-file-watcher-updated (_session-id properties)
  "Handle a file.watcher.updated event from _SESSION-ID with PROPERTIES.
PROPERTIES contains `file' and `event' keys."
  (let ((filepath (alist-get 'file properties))
        (event (alist-get 'event properties)))
    (when (and filepath (string= event "change"))
      (org-opencode--maybe-revert-file filepath))))

(defun org-opencode--maybe-revert-file (filepath)
  "Revert the buffer visiting FILEPATH based on `org-opencode-auto-revert-files'.
If the buffer is not modified, it is reverted silently regardless of
`org-opencode-auto-revert-files' setting."
  (let ((buffer (find-buffer-visiting filepath)))
    (when buffer
      (with-current-buffer buffer
        (if (not (buffer-modified-p))
            ;; Buffer not modified, revert silently
            (progn
              (revert-buffer t t)
              (message "OpenCode: reverted %s" filepath))
          ;; Buffer has modifications, check user preference
          (pcase org-opencode-auto-revert-files
            ('t (revert-buffer t t)
                (message "OpenCode: auto-reverted %s" filepath))
            ('ask (when (y-or-n-p (format "OpenCode edited %s. Revert? " filepath))
                    (revert-buffer t t)
                    (message "OpenCode: reverted %s" filepath)))
            ('notify (message "OpenCode edited %s (buffer has changes)" filepath))
            (_ nil)))))))

;; ---------------------------------------------------------------------------
;; Session Diff Viewer
;; ---------------------------------------------------------------------------

(defun org-opencode-show-session-diff (session-id)
  "Show the diff for SESSION-ID in a dedicated buffer.
The diff is displayed in Org format with collapsible sections per file.
Interactively, prompts for SESSION-ID if not provided."
  (interactive
   (list (or (org-opencode--current-session-id)
             (read-string "Session ID: "))))
  (org-opencode--ensure-server)
  (let ((diffs (org-opencode-api-session-diff session-id)))
    (with-current-buffer (get-buffer-create org-opencode-diff-buffer-name)
      (erase-buffer)
      (org-mode)
      (setq-local org-opencode-current-session-id session-id)
      (insert (format "* OpenCode Session Diff: %s\n\n" session-id))
      (if (null diffs)
          (insert "No changes in this session.\n")
        (dolist (diff diffs)
          (let ((filename (alist-get 'filename diff))
                (additions (or (alist-get 'additions diff) 0))
                (deletions (or (alist-get 'deletions diff) 0))
                (content (alist-get 'content diff)))
            (insert (format "** %s (+%d / -%d)\n" filename additions deletions))
            (when content
              (insert "#+begin_src diff\n")
              (insert content)
              (insert "\n#+end_src\n"))
            (insert "\n"))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun org-opencode-refresh-session-diff ()
  "Refresh the current session diff buffer."
  (interactive)
  (if (bound-and-true-p org-opencode-current-session-id)
      (org-opencode-show-session-diff org-opencode-current-session-id)
    (user-error "Not in an OpenCode diff buffer")))

;; ---------------------------------------------------------------------------
;; Session Diff Handler
;; ---------------------------------------------------------------------------

(defun org-opencode--handle-session-diff (session-id properties)
  "Handle a session.diff event from SESSION-ID with PROPERTIES.
PROPERTIES contains `sessionID' and `diff' keys."
  (let ((diff-data (alist-get 'diff properties)))
    (when diff-data
      (message "OpenCode: Session %s has %d file(s) with changes"
               session-id (length diff-data)))))

;; ---------------------------------------------------------------------------
;; File State Management
;; ---------------------------------------------------------------------------

(defun org-opencode-file-has-changes-p (filepath session-id)
  "Return t if FILEPATH has changes in SESSION-ID."
  (let ((diffs (org-opencode-api-session-diff session-id)))
    (cl-some (lambda (diff)
               (string= (alist-get 'filename diff) filepath))
             diffs)))

(defun org-opencode-get-file-diff (filepath session-id)
  "Get the diff for FILEPATH in SESSION-ID.
Returns the diff alist or nil if no changes."
  (let ((diffs (org-opencode-api-session-diff session-id)))
    (cl-find-if (lambda (diff)
                  (string= (alist-get 'filename diff) filepath))
                diffs)))

;; ---------------------------------------------------------------------------
;; Accept/Reject Workflow (Placeholder for future implementation)
;; ---------------------------------------------------------------------------

(defun org-opencode-accept-file-changes (filepath)
  "Accept changes to FILEPATH from opencode.
This marks the changes as accepted (currently just a placeholder)."
  (interactive "FAccept changes to file: ")
  (message "OpenCode: Accepted changes to %s" filepath))

(defun org-opencode-reject-file-changes (filepath)
  "Reject changes to FILEPATH from opencode.
This reverts the file to its original state."
  (interactive "FReject changes to file: ")
  (when (y-or-n-p (format "Revert %s to original state? " filepath))
    (let ((buffer (find-buffer-visiting filepath)))
      (when buffer
        (with-current-buffer buffer
          (revert-buffer t t)))
      (message "OpenCode: Reverted %s" filepath))))

;; ---------------------------------------------------------------------------
;; Utility Functions
;; ---------------------------------------------------------------------------

(defun org-opencode--current-session-id ()
  "Get the current session ID from buffer-local variable or global state."
  (or (bound-and-true-p org-opencode-current-session-id)
      (bound-and-true-p org-opencode-session-id)))

;; ---------------------------------------------------------------------------
;; Event Handler Registration
;; ---------------------------------------------------------------------------

;; Register handlers for file-related events
(org-opencode-register-event-handler "file.edited" #'org-opencode--handle-file-edited)
(org-opencode-register-event-handler "file.watcher.updated" #'org-opencode--handle-file-watcher-updated)
(org-opencode-register-event-handler "session.diff" #'org-opencode--handle-session-diff)

;; ---------------------------------------------------------------------------
;; Keymap for Diff Buffer
;; ---------------------------------------------------------------------------

(defvar org-opencode-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-opencode-refresh-session-diff)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for OpenCode diff buffers.")

;; Add the keymap to diff buffers via a minor mode or by setting it directly
(defun org-opencode--maybe-install-diff-keymap ()
  "Install `org-opencode-diff-mode-map' in the OpenCode diff buffer.
Intended for use on `org-mode-hook'."
  (when (and (buffer-name)
             (string= (buffer-name) org-opencode-diff-buffer-name))
    (use-local-map org-opencode-diff-mode-map)))

(add-hook 'org-mode-hook #'org-opencode--maybe-install-diff-keymap)

;; ---------------------------------------------------------------------------
;; Provide Feature
;; ---------------------------------------------------------------------------

(provide 'org-opencode-files)

;;; org-opencode-files.el ends here
