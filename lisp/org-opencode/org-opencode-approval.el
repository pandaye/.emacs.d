;;; org-opencode-approval.el --- Tool approval UI for org-opencode -*- lexical-binding: t; -*-

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

;; This module implements tool approval UI for the opencode AI agent.
;; When the agent requests permission to perform dangerous operations
;; (file writes, shell commands, etc.), this module shows a prompt
;; and sends the approval/rejection back via the API.

;;; Code:

(require 'org-opencode-core)
(require 'org-opencode-events)
(require 'org-opencode-session)

;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

(defvar org-opencode--pending-permissions nil
  "Alist of (SESSION-ID . PERMISSION-LIST) for pending approval requests.
Each permission in PERMISSION-LIST is an alist with keys:
id, type, pattern, title, metadata.")

(defvar org-opencode-auto-approve-rules nil
  "Rules for automatically approving permissions.
Each rule is (TYPE . ACTION) where TYPE is the permission type string
and ACTION is one of: \\='once\\=', \\='always\\=', or \\='reject\\='.

Example:
  \\='((\"read\" . \"once\") (\"write\" . \"reject\"))

Useful for auto-approving read operations while prompting for writes.
Note that \\='always\\=' adds to a permanent list that survives restarts.")

(defvar org-opencode-auto-approve-patterns nil
  "List of regex patterns for auto-approving.
Each element is a cons cell (REGEX . ACTION) where REGEX is a string
regex pattern and ACTION is \\='once\\=', \\='always\\=', or \\='reject\\='.

If a permission\\='s title matches any pattern, it is auto-approved
with the specified action.")

(defvar org-opencode--auto-approved-types nil
  "Types that have been auto-approved with \\='always\\='.
This is persisted and consulted before prompting.")

;;------------------------------------------------------------------------------
;; Auto-Approval Logic
;;------------------------------------------------------------------------------

(defun org-opencode--check-auto-approve (type _title _pattern)
  "Check if permission TYPE should be auto-approved.
Returns the action (\"once\", \"always\", or \"reject\") if it matches
an auto-approval rule, or nil if it should prompt the user."
  ;; Check type-based rules
  (let ((type-rule (assoc type org-opencode-auto-approve-rules)))
    (cond
     (type-rule (cdr type-rule))
     ;; Check permanently auto-approved types
     ((member type org-opencode--auto-approved-types) "once")
     ;; No auto-approval matched
     (t nil))))

;;------------------------------------------------------------------------------
;; Permission Prompt UI
;;------------------------------------------------------------------------------

(defun org-opencode--add-pending-permission (session-id permission)
  "Add PERMISSION to the pending list for SESSION-ID.
PERMISSION is an alist with permission data."
  ;; Store in a global alist keyed by session-id
  (let ((existing (assoc session-id org-opencode--pending-permissions)))
    (if existing
        (setcdr existing (cons permission (cdr existing)))
      (push (cons session-id (list permission)) org-opencode--pending-permissions))))

(defun org-opencode--remove-pending-permission (session-id perm-id)
  "Remove the permission with ID PERM-ID from SESSION-ID's pending list."
  (let ((existing (assoc session-id org-opencode--pending-permissions)))
    (when existing
      (setcdr existing (cl-remove-if (lambda (p) (equal (alist-get 'id p) perm-id))
                                      (cdr existing))))))

(defun org-opencode--prompt-for-approval (session-id perm-id perm-type title pattern)
  "Prompt the user to approve or reject a permission request.

SESSION-ID identifies the session,
PERM-ID is the permission identifier,
PERM-TYPE is the operation type (e.g., \\='write\\=', \\='bash\\='),
TITLE is a description of the operation,
PATTERN describes what files/commands are affected."
  (let* ((prompt-str (format "OpenCode permission: %s%s\n[o]nce / [a]lways / [r]eject? "
                             (or title "Unknown operation")
                             (if pattern (format " (%s)" pattern) "")))
         (valid-keys '(?o ?a ?r))
         (actions '(("o" . "once") ("a" . "always") ("r" . "reject")))
         choice response)
    ;; Show prompt in minibuffer
    (condition-case nil
        (progn
          (setq choice (read-char-choice prompt-str valid-keys))
          (setq response (cdr (assoc (char-to-string choice) actions)))
          (when response
            ;; Handle 'always' - remember this type for auto-approval
            (when (string= response "always")
              (cl-pushnew perm-type org-opencode--auto-approved-types :test #'string=))
            ;; Send the response to the API
            (org-opencode-api-approve-permission session-id perm-id response)
            ;; Remove from pending and update display
            (org-opencode--remove-pending-permission session-id perm-id)
            ;; Provide feedback
            (if (string= response "reject")
                (message "OpenCode: Rejected %s" (or title perm-type))
              (message "OpenCode: Approved %s%s"
                       (or title perm-type)
                       (if (string= response "always") " (always)" "")))))
      (quit
       ;; User canceled with C-g - treat as reject
       (org-opencode-api-approve-permission session-id perm-id "reject")
       (org-opencode--remove-pending-permission session-id perm-id)
       (message "OpenCode: Rejected %s (cancelled)" (or title perm-type))))))

;;------------------------------------------------------------------------------
;; Event Handlers
;;------------------------------------------------------------------------------

(defun org-opencode--handle-permission-updated (session-id properties)
  "Handle a permission.updated event.

SESSION-ID identifies the session.
PROPERTIES is an alist containing:
  - id: permission identifier
  - type: operation type (write, edit, bash, etc.)
  - pattern: what files/commands are affected
  - title: description of the operation
  - metadata: additional info
  - time: timestamp
  - messageID: associated message"
  (let* ((perm-id (alist-get 'id properties))
         (perm-type (alist-get 'type properties))
         (perm-title (alist-get 'title properties))
         (perm-pattern (alist-get 'pattern properties))
         (perm-metadata (alist-get 'metadata properties))
         (perm-time (alist-get 'time properties))
         ;; Store permission data
         (permission `((id . ,perm-id)
                       (type . ,perm-type)
                       (pattern . ,perm-pattern)
                       (title . ,perm-title)
                       (metadata . ,perm-metadata)
                       (time . ,perm-time)))
         ;; Check auto-approval
         (auto-action (org-opencode--check-auto-approve perm-type perm-title perm-pattern)))
    ;; Add to pending list regardless
    (org-opencode--add-pending-permission session-id permission)
    ;; Either auto-approve or prompt
    (if auto-action
        (progn
          (org-opencode-api-approve-permission session-id perm-id auto-action)
          (org-opencode--remove-pending-permission session-id perm-id)
          (message "OpenCode: Auto-approved %s (%s: %s)"
                   (or perm-title perm-type) perm-type auto-action))
      ;; No auto-approval - prompt the user
      (org-opencode--prompt-for-approval session-id perm-id perm-type perm-title perm-pattern))))

(defun org-opencode--handle-permission-replied (session-id properties)
  "Handle a permission.replied event.

SESSION-ID identifies the session.
PROPERTIES is an alist containing:
  - permissionID: the permission that was replied to
  - response: the response given (once/always/reject)"
  (let ((perm-id (alist-get 'permissionID properties))
        (response (alist-get 'response properties)))
    ;; Remove from pending list
    (org-opencode--remove-pending-permission session-id perm-id)
    (message "OpenCode: Permission %s response: %s" perm-id response)))

;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun org-opencode-approval-clear-pending (session-id)
  "Clear all pending permissions for SESSION-ID.
This can be useful if the session gets into a bad state."
  (interactive (list (or (org-opencode--session-id) "")))
  (let ((existing (assoc session-id org-opencode--pending-permissions)))
    (when existing
      (setcdr existing nil)))
  (message "OpenCode: Cleared pending permissions for session %s" session-id))

(defun org-opencode-approval-show-pending (session-id)
  "Display all pending permissions for SESSION-ID."
  (interactive (list (or (org-opencode--session-id) "")))
  (let ((existing (assoc session-id org-opencode--pending-permissions)))
    (if (or (not existing) (null (cdr existing)))
        (message "OpenCode: No pending permissions for session %s" session-id)
      (with-output-to-temp-buffer "*OpenCode Pending Permissions*"
        (princ (format "Pending permissions for session %s:\n\n" session-id))
        (dolist (perm (cdr existing))
          (princ (format "ID: %s\n" (alist-get 'id perm)))
          (princ (format "  Type: %s\n" (alist-get 'type perm)))
          (princ (format "  Title: %s\n" (or (alist-get 'title perm) "N/A")))
          (princ (format "  Pattern: %s\n" (or (alist-get 'pattern perm) "N/A")))
          (princ "\n"))))))

;;------------------------------------------------------------------------------
;; Register Event Handlers
;;------------------------------------------------------------------------------

(org-opencode-register-event-handler "permission.updated" #'org-opencode--handle-permission-updated)
(org-opencode-register-event-handler "permission.replied" #'org-opencode--handle-permission-replied)

(provide 'org-opencode-approval)
;;; org-opencode-approval.el ends here
