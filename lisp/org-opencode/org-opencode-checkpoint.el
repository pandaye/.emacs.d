;;; org-opencode-checkpoint.el --- Checkpoint/rollback for org-opencode -*- lexical-binding: t; -*-

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

;; Two-layer checkpoint/rollback for org-opencode:
;;
;; 1. Frontend layer: `git stash create' before each send to snapshot the
;;    working tree.  This allows per-file restore via `git show REF:path'.
;;
;; 2. Server layer: `POST /session/{id}/revert' and `/unrevert' for
;;    message-level undo/redo in the conversation history.
;;
;; The frontend checkpoint is created automatically in `org-opencode-send'
;; (via `org-opencode--checkpoint-before-send').  After the agent finishes,
;; the ediff review flow (in org-opencode-files.el) uses the checkpoint ref
;; to compare pre-send and post-send file states.

;;; Code:

(require 'org-opencode-core)

;; ---------------------------------------------------------------------------
;; Buffer-Local State
;; ---------------------------------------------------------------------------

(defvar-local org-opencode--checkpoint-ref nil
  "Git object ref from `git stash create' taken before the last send.
This is a full SHA-1 hash, not a stash index.  It is never pushed
to the stash reflog, so it does not pollute `git stash list'.")

(defvar-local org-opencode--checkpoint-directory nil
  "Absolute path of the git working tree where the checkpoint was created.")

;; ---------------------------------------------------------------------------
;; Frontend Checkpoint (git stash create)
;; ---------------------------------------------------------------------------

(defun org-opencode--checkpoint-create ()
  "Snapshot the working tree via `git stash create'.

Returns the stash object ref (SHA) on success, or nil when:
- The session directory is not inside a git repository.
- The working tree is clean (nothing to snapshot).

The ref is stored in `org-opencode--checkpoint-ref' for later use
by the ediff review and accept/reject workflows."
  (let ((dir (org-opencode--session-directory)))
    (when dir
      (let* ((default-directory dir)
             (ref (string-trim
                   (with-output-to-string
                     (with-current-buffer standard-output
                       (process-file "git" nil t nil
                                     "stash" "create"
                                     "org-opencode checkpoint"))))))
        (if (and ref (not (string-empty-p ref)))
            (progn
              (setq org-opencode--checkpoint-ref ref
                    org-opencode--checkpoint-directory dir)
              ref)
          ;; Clean tree — stash create returns empty string.
          ;; Still record the directory so we know where we are, but use
          ;; HEAD as the reference point.
          (let ((head (string-trim
                       (with-output-to-string
                         (with-current-buffer standard-output
                           (process-file "git" nil t nil
                                         "rev-parse" "HEAD"))))))
            (when (and head (not (string-empty-p head)))
              (setq org-opencode--checkpoint-ref head
                    org-opencode--checkpoint-directory dir)
              head)))))))

(defun org-opencode--checkpoint-get-file-content (filepath)
  "Return the content of FILEPATH at the checkpoint ref as a string.
Returns nil when no checkpoint exists or the file did not exist at
that point."
  (when (and org-opencode--checkpoint-ref org-opencode--checkpoint-directory)
    (let* ((default-directory org-opencode--checkpoint-directory)
           (relative (file-relative-name
                      (expand-file-name filepath)
                      org-opencode--checkpoint-directory))
           (output (with-output-to-string
                     (with-current-buffer standard-output
                       (process-file "git" nil t nil
                                     "show"
                                     (format "%s:%s"
                                             org-opencode--checkpoint-ref
                                             relative))))))
      (unless (string-empty-p output)
        output))))

(defun org-opencode--checkpoint-restore-file (filepath)
  "Restore FILEPATH to its state at the checkpoint ref.
This overwrites the file on disk.  Any buffer visiting the file
is reverted afterwards."
  (when (and org-opencode--checkpoint-ref org-opencode--checkpoint-directory)
    (let* ((default-directory org-opencode--checkpoint-directory)
           (relative (file-relative-name
                      (expand-file-name filepath)
                      org-opencode--checkpoint-directory)))
      (with-temp-buffer
        (unless (zerop (process-file "git" nil t nil
                                     "checkout"
                                     org-opencode--checkpoint-ref
                                     "--" relative))
          (error "Failed to restore %s from checkpoint: %s"
                 relative (buffer-string))))
      ;; Revert any open buffer visiting the file.
      (let ((buf (find-buffer-visiting filepath)))
        (when buf
          (with-current-buffer buf
            (revert-buffer t t)))))))

(defun org-opencode--checkpoint-clear ()
  "Clear the current checkpoint state."
  (setq org-opencode--checkpoint-ref nil
        org-opencode--checkpoint-directory nil))

(defun org-opencode--checkpoint-before-send ()
  "Hook called before `org-opencode-send' to create a checkpoint.
Designed to be added to `org-opencode-before-send-hook'."
  (let ((ref (org-opencode--checkpoint-create)))
    (when ref
      (message "OpenCode: checkpoint %s created" (substring ref 0 (min 8 (length ref)))))))

;; ---------------------------------------------------------------------------
;; Server-Side Revert / Unrevert
;; ---------------------------------------------------------------------------

(defun org-opencode-revert-last-message ()
  "Revert the last assistant message via the opencode server API.
This removes the last message from the conversation history on the
server side.  Use `org-opencode-unrevert-last-message' to undo."
  (interactive)
  (unless (org-opencode--session-id)
    (user-error "No active opencode session"))
  (org-opencode--ensure-server)
  (org-opencode-api-revert (org-opencode--session-id))
  (message "OpenCode: reverted last message in session %s"
           (org-opencode--session-id)))

(defun org-opencode-unrevert-last-message ()
  "Undo the last revert, restoring the reverted message.
Only works if the last operation was a revert."
  (interactive)
  (unless (org-opencode--session-id)
    (user-error "No active opencode session"))
  (org-opencode--ensure-server)
  (org-opencode-api-unrevert (org-opencode--session-id))
  (message "OpenCode: unreverted message in session %s"
           (org-opencode--session-id)))

(provide 'org-opencode-checkpoint)
;;; org-opencode-checkpoint.el ends here
