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
;; 3. Reviews file diffs via ediff (checkpoint vs current)
;; 4. Provides accept/reject workflow backed by git checkpoint restore
;;
;; Diff review is triggered after the agent completes (session.idle).
;; Each changed file can be reviewed individually in ediff, then
;; accepted (keep agent's version) or rejected (restore checkpoint).

;;; Code:

(require 'org-opencode-core)
(require 'org-opencode-events)
(require 'ediff)

;; Forward declarations for checkpoint module
(declare-function org-opencode--checkpoint-get-file-content "org-opencode-checkpoint")
(declare-function org-opencode--checkpoint-restore-file "org-opencode-checkpoint")

;; Forward declaration for session module
(declare-function org-opencode--session-id "org-opencode-session")

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

(defcustom org-opencode-auto-review-on-idle t
  "When non-nil, prompt to review diffs when the agent finishes.
The prompt only appears if the session has file changes and a
checkpoint ref is available."
  :type 'boolean
  :group 'org-opencode-files)

(defcustom org-opencode-diff-buffer-name "*opencode-diff*"
  "Name of the buffer used to display the session diff summary."
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
;; Session Diff Summary (quick overview, non-ediff)
;; ---------------------------------------------------------------------------

(defun org-opencode-show-session-diff (session-id)
  "Show a summary of changed files for SESSION-ID.
Displays file names with addition/deletion counts.  For detailed
per-file review, use `org-opencode-review-session-diff' instead.
Interactively, uses the current session."
  (interactive
   (list (or (org-opencode--current-session-id)
             (read-string "Session ID: "))))
  (org-opencode--ensure-server)
  (let ((diffs (org-opencode-api-session-diff session-id)))
    (with-current-buffer (get-buffer-create org-opencode-diff-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "OpenCode Session Diff: %s\n" session-id))
        (insert (make-string 60 ?-) "\n\n")
        (setq-local org-opencode-current-session-id session-id)
        (if (null diffs)
            (insert "No changes in this session.\n")
          (dolist (diff diffs)
            (let ((filename (alist-get 'filename diff))
                  (additions (or (alist-get 'additions diff) 0))
                  (deletions (or (alist-get 'deletions diff) 0)))
              (insert (format "  %s  (+%d / -%d)\n" filename additions deletions))))
          (insert (format "\n%d file(s) changed.\n" (length diffs)))
          (insert "\nPress 'e' to review with ediff, 'a' to accept all, 'q' to quit.\n"))
        (goto-char (point-min)))
      (setq buffer-read-only t)
      (use-local-map org-opencode-diff-mode-map)
      (display-buffer (current-buffer)))))

(defun org-opencode-refresh-session-diff ()
  "Refresh the current session diff buffer."
  (interactive)
  (if (bound-and-true-p org-opencode-current-session-id)
      (org-opencode-show-session-diff org-opencode-current-session-id)
    (user-error "Not in an OpenCode diff buffer")))

;; ---------------------------------------------------------------------------
;; Ediff-Based Review
;; ---------------------------------------------------------------------------

(defvar org-opencode--review-queue nil
  "List of (FILEPATH . SESSION-ID) pairs awaiting ediff review.")

(defvar org-opencode--review-current nil
  "The (FILEPATH . SESSION-ID) currently being reviewed in ediff, or nil.")

(defun org-opencode-review-session-diff (&optional session-id)
  "Review all changed files in SESSION-ID one-by-one using ediff.
For each file, ediff compares the checkpoint version (before send)
against the current version (after agent edits).

In the ediff control panel:
- `a' copies the checkpoint (old) version — effectively rejecting the change.
- `b' copies the agent (new) version — effectively accepting the change.
- `q' quits ediff; you are then prompted to accept or reject.

SESSION-ID defaults to the current buffer's session."
  (interactive)
  (let ((sid (or session-id
                 (org-opencode--current-session-id)
                 (bound-and-true-p org-opencode--session)
                 (read-string "Session ID: "))))
    (when (and (listp sid) (alist-get 'id sid))
      (setq sid (alist-get 'id sid)))
    (when (string-empty-p sid)
      (user-error "Session ID cannot be empty"))
    (org-opencode--ensure-server)
    (let* ((diffs (org-opencode-api-session-diff sid))
           (files (mapcar (lambda (d) (alist-get 'filename d)) diffs)))
      (if (null files)
          (message "OpenCode: No file changes to review")
        (setq org-opencode--review-queue
              (mapcar (lambda (f) (cons f sid)) files))
        (setq org-opencode--review-current nil)
        (message "OpenCode: %d file(s) to review" (length files))
        (org-opencode--review-next)))))

(defun org-opencode--review-next ()
  "Pop the next file from the review queue and open ediff for it."
  (if (null org-opencode--review-queue)
      (progn
        (setq org-opencode--review-current nil)
        (message "OpenCode: diff review complete"))
    (let* ((entry (pop org-opencode--review-queue))
           (filepath (car entry))
           (session-id (cdr entry)))
      (setq org-opencode--review-current entry)
      (org-opencode--ediff-review-file filepath session-id))))

(defun org-opencode--ediff-review-file (filepath _session-id)
  "Launch ediff comparing checkpoint vs current version of FILEPATH.
_SESSION-ID is kept for future use but not needed for the diff itself."
  (let ((old-content (org-opencode--checkpoint-get-file-content filepath)))
    (unless old-content
      ;; File didn't exist at checkpoint — it's a new file.
      (setq old-content ""))
    (let* ((buf-old (generate-new-buffer
                     (format "*checkpoint:%s*"
                             (file-name-nondirectory filepath))))
           (buf-new (or (find-buffer-visiting filepath)
                        (find-file-noselect filepath t))))
      ;; Fill the checkpoint buffer with old content.
      (with-current-buffer buf-old
        (insert old-content)
        ;; Inherit the mode from the file for syntax highlighting.
        (let ((buffer-file-name filepath))
          (ignore-errors (set-auto-mode)))
        (setq buffer-read-only t)
        (goto-char (point-min)))
      ;; Make sure the current file buffer is up to date.
      (with-current-buffer buf-new
        (when (not (buffer-modified-p))
          (revert-buffer t t)))
      ;; Store filepath for the quit hook.
      (setq org-opencode--ediff-filepath filepath)
      ;; Launch ediff.
      (ediff-buffers
       buf-old buf-new
       (list (lambda ()
               (setq-local ediff-quit-hook
                           (list #'org-opencode--ediff-quit-handler
                                 #'ediff-cleanup-mess))))))))

(defvar org-opencode--ediff-filepath nil
  "Filepath of the file being reviewed in the current ediff session.")

(defun org-opencode--ediff-quit-handler ()
  "Handle ediff quit: prompt accept/reject, clean up, advance to next file."
  (let ((filepath org-opencode--ediff-filepath)
        (buf-old ediff-buffer-A))
    ;; Kill the temporary checkpoint buffer.
    (when (buffer-live-p buf-old)
      (kill-buffer buf-old))
    ;; Prompt for accept/reject.
    (when filepath
      (let ((action (read-char-choice
                     (format "OpenCode %s: [a]ccept / [r]eject? "
                             (file-name-nondirectory filepath))
                     '(?a ?r))))
        (pcase action
          (?a (org-opencode-accept-file-changes filepath))
          (?r (org-opencode-reject-file-changes filepath)))))
    ;; Advance to next file.
    (run-with-timer 0.1 nil #'org-opencode--review-next)))

;; ---------------------------------------------------------------------------
;; Accept/Reject Workflow
;; ---------------------------------------------------------------------------

(defun org-opencode-accept-file-changes (filepath)
  "Accept the agent's changes to FILEPATH.
This keeps the current file content (agent's version) and reverts
any open buffer to ensure it reflects the on-disk state."
  (interactive
   (list (read-file-name "Accept changes to file: "
                         (org-opencode--session-directory))))
  (let ((buffer (find-buffer-visiting filepath)))
    (when buffer
      (with-current-buffer buffer
        (revert-buffer t t))))
  (message "OpenCode: accepted changes to %s" (file-name-nondirectory filepath)))

(defun org-opencode-reject-file-changes (filepath)
  "Reject the agent's changes to FILEPATH.
Restores the file to its state at the checkpoint (before the last
send).  Requires a checkpoint ref from `org-opencode-checkpoint'."
  (interactive
   (list (read-file-name "Reject changes to file: "
                         (org-opencode--session-directory))))
  (require 'org-opencode-checkpoint)
  (if (not (bound-and-true-p org-opencode--checkpoint-ref))
      (user-error "No checkpoint available; cannot reject changes")
    (org-opencode--checkpoint-restore-file filepath)
    (message "OpenCode: rejected changes to %s (restored from checkpoint)"
             (file-name-nondirectory filepath))))

(defun org-opencode-accept-all-changes (&optional session-id)
  "Accept all file changes in SESSION-ID.
Reverts all open buffers for changed files."
  (interactive)
  (let ((sid (or session-id (org-opencode--current-session-id))))
    (unless sid (user-error "No active session"))
    (let ((diffs (org-opencode-api-session-diff sid)))
      (dolist (diff diffs)
        (org-opencode-accept-file-changes (alist-get 'filename diff)))
      (message "OpenCode: accepted all %d file changes" (length diffs)))))

(defun org-opencode-reject-all-changes (&optional session-id)
  "Reject all file changes in SESSION-ID.
Restores all changed files from the checkpoint."
  (interactive)
  (let ((sid (or session-id (org-opencode--current-session-id))))
    (unless sid (user-error "No active session"))
    (require 'org-opencode-checkpoint)
    (unless (bound-and-true-p org-opencode--checkpoint-ref)
      (user-error "No checkpoint available"))
    (let ((diffs (org-opencode-api-session-diff sid)))
      (dolist (diff diffs)
        (org-opencode-reject-file-changes (alist-get 'filename diff)))
      (message "OpenCode: rejected all %d file changes" (length diffs)))))

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
;; Auto-Review on Session Idle
;; ---------------------------------------------------------------------------

(defun org-opencode--handle-session-idle-for-review (session-id _properties)
  "Offer diff review when the agent finishes, if changes exist.
Called from the `session.idle' event.  Only prompts when
`org-opencode-auto-review-on-idle' is non-nil and a checkpoint
ref is available."
  (when (and org-opencode-auto-review-on-idle
             (bound-and-true-p org-opencode--checkpoint-ref))
    ;; Check if the idle session matches our buffer's session.
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (bound-and-true-p org-opencode-mode)
                     (equal (org-opencode--session-id) session-id))
            ;; Schedule prompt outside the event filter context.
            (run-with-timer
             0.5 nil
             (lambda (sid)
               (condition-case nil
                   (let ((diffs (org-opencode-api-session-diff sid)))
                     (when (and diffs (> (length diffs) 0))
                       (when (y-or-n-p
                              (format "OpenCode: %d file(s) changed. Review diffs? "
                                      (length diffs)))
                         (org-opencode-review-session-diff sid))))
                 (error nil)))
             session-id)))))))

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
;; Utility Functions
;; ---------------------------------------------------------------------------

(defun org-opencode--current-session-id ()
  "Get the current session ID from buffer-local variable or global state."
  (or (bound-and-true-p org-opencode-current-session-id)
      (bound-and-true-p org-opencode-session-id)))

;; ---------------------------------------------------------------------------
;; Event Handler Registration
;; ---------------------------------------------------------------------------

(org-opencode-register-event-handler "file.edited" #'org-opencode--handle-file-edited)
(org-opencode-register-event-handler "file.watcher.updated" #'org-opencode--handle-file-watcher-updated)
(org-opencode-register-event-handler "session.diff" #'org-opencode--handle-session-diff)
(org-opencode-register-event-handler "session.idle" #'org-opencode--handle-session-idle-for-review)

;; ---------------------------------------------------------------------------
;; Keymap for Diff Summary Buffer
;; ---------------------------------------------------------------------------

(defvar org-opencode-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'org-opencode--diff-buffer-review)
    (define-key map (kbd "a") #'org-opencode--diff-buffer-accept-all)
    (define-key map (kbd "g") #'org-opencode-refresh-session-diff)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for OpenCode diff summary buffers.

\\<org-opencode-diff-mode-map>
  e  review with ediff
  a  accept all changes
  g  refresh
  q  quit")

(defun org-opencode--diff-buffer-review ()
  "Launch ediff review from the diff summary buffer."
  (interactive)
  (when (bound-and-true-p org-opencode-current-session-id)
    (org-opencode-review-session-diff org-opencode-current-session-id)))

(defun org-opencode--diff-buffer-accept-all ()
  "Accept all changes from the diff summary buffer."
  (interactive)
  (when (bound-and-true-p org-opencode-current-session-id)
    (org-opencode-accept-all-changes org-opencode-current-session-id)))

;; ---------------------------------------------------------------------------
;; Provide Feature
;; ---------------------------------------------------------------------------

(provide 'org-opencode-files)

;;; org-opencode-files.el ends here
