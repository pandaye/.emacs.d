;;; org-opencode-ui.el --- UI interactions for org-opencode -*- lexical-binding: t; -*-

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

;; This module provides user interaction UI for org-opencode:
;; - Model and agent interactive selection
;; - Prompt reading (region, headline, minibuffer)
;; - Model/agent variable management
;;
;; Status display, header-line, and response rendering live in
;; org-opencode-render.el.

;;; Code:

(require 'org)
(require 'org-opencode-core)

;; Forward declaration — org-opencode-send is defined in org-opencode.el
;; which requires this module, so we cannot require it here.
(declare-function org-opencode-send "org-opencode")

;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defcustom org-opencode-send-headline-by-default t
  "When non-nil, `org-opencode-send' uses current headline as default prompt.

Prompt source priority is: active region, current headline content, then
minibuffer. Use prefix argument to force minibuffer input."
  :type 'boolean
  :group 'org-opencode)

(defvar org-opencode-prompt-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "@") #'org-opencode--minibuffer-insert-at-file)
    map)
  "Keymap active when reading an opencode prompt in the minibuffer.
Binds `@' to insert a file reference via completion.")

;; -----------------------------------------------------------------------------
;; Selected Model/Agent Variables
;; -----------------------------------------------------------------------------

(defvar-local org-opencode-selected-model nil
  "Selected model for the current session, or nil for default.
Format: \"provider/model\" string.")

(defvar-local org-opencode-selected-agent nil
  "Selected agent for the current session, or nil for default.")

;; -----------------------------------------------------------------------------
;; Prompt Reading
;; -----------------------------------------------------------------------------

(defun org-opencode--minibuffer-insert-at-file ()
  "Insert an @file reference at point in the minibuffer.
Triggers `read-file-name' to select a file, then inserts it as
@relative-path at point."
  (interactive)
  (let* ((dir (or (org-opencode--session-directory) default-directory))
         (file (read-file-name "Attach file: " dir nil t)))
    (when file
      (insert "@" (file-relative-name file dir)))))

(defun org-opencode--read-prompt (force-minibuffer)
  "Return a prompt for opencode.
When FORCE-MINIBUFFER is non-nil, always read from the minibuffer.
Otherwise, use the active region when available."
  (let ((region-text
         (unless force-minibuffer
           (when (use-region-p)
             (string-trim
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))))))
        (headline-text
         (unless force-minibuffer
           (when org-opencode-send-headline-by-default
             (org-opencode--headline-prompt)))))
    (or (and region-text
             (not (string-empty-p region-text))
             region-text)
        (and headline-text
             (not (string-empty-p headline-text))
             headline-text)
        (string-trim (read-from-minibuffer "Prompt for opencode: " nil org-opencode-prompt-minibuffer-map)))))

(defun org-opencode--headline-prompt ()
  "Return prompt text built from current headline and section text.

Returns nil when point is before the first heading."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (unless (org-before-first-heading-p)
        (org-back-to-heading t)
        (let* ((title (string-trim (or (org-get-heading t t t t) "")))
               (section-start (progn (org-end-of-meta-data t) (point)))
               (section-end (save-excursion
                              (if (re-search-forward org-outline-regexp-bol nil t)
                                  (match-beginning 0)
                                (point-max))))
               (body (string-trim
                      (buffer-substring-no-properties section-start section-end))))
          (cond
           ((and (not (string-empty-p title)) (not (string-empty-p body)))
            (format "%s\n\n%s" title body))
           ((not (string-empty-p body)) body)
           ((not (string-empty-p title)) title)
           (t nil)))))))

;; -----------------------------------------------------------------------------
;; Model Selection
;; -----------------------------------------------------------------------------

(defun org-opencode-select-model ()
  "Interactively select a model for the current session.
Shows available models from all connected providers and lets the user pick one."
  (interactive)
  (org-opencode--ensure-server)
  (let* ((providers (org-opencode-api-providers))
         (connected (alist-get 'connected providers))
         (all-providers (alist-get 'all providers))
         (defaults (alist-get 'default providers))
         (models (org-opencode--flatten-models all-providers connected defaults))
         (choice (completing-read "Select model: " models nil t)))
    (when (and choice (not (string-empty-p choice)))
      ;; Strip default marker " *" if present
      (let ((clean (replace-regexp-in-string " \\*$" "" choice)))
        (setq-local org-opencode-selected-model clean)
        (message "OpenCode model: %s" clean)))))

(defun org-opencode--flatten-models (all-providers connected defaults)
  "Flatten ALL-PROVIDERS into a list of \"provider/model\" strings.
Only include providers in CONNECTED list.
DEFAULTS is an alist mapping provider-id to default model name;
default models are marked with a star."
  (let (result)
    (dolist (provider all-providers)
      (let* ((provider-id (alist-get 'id provider))
             (provider-models (alist-get 'models provider)))
        (when (member provider-id connected)
          (dolist (model-entry (if (listp provider-models) provider-models nil))
            (let* ((model-id (cond
                              ((stringp model-entry) model-entry)
                              ((listp model-entry) (or (alist-get 'id model-entry)
                                                       (alist-get 'name model-entry)))
                              (t (format "%s" model-entry))))
                   (is-default (and defaults
                                    (equal model-id (alist-get (intern provider-id) defaults))))
                   (display (format "%s/%s%s" provider-id model-id
                                    (if is-default " *" ""))))
              (push display result))))))
    (nreverse result)))

;; -----------------------------------------------------------------------------
;; Agent Selection
;; -----------------------------------------------------------------------------

(defun org-opencode-select-agent ()
  "Interactively select an agent for the current session."
  (interactive)
  (org-opencode--ensure-server)
  (let* ((agents (org-opencode-api-agents))
         (names (mapcar (lambda (a) (or (alist-get 'name a) (alist-get 'id a))) agents))
         (choice (completing-read "Select agent: " names nil t)))
    (when (and choice (not (string-empty-p choice)))
      (setq-local org-opencode-selected-agent choice)
      (message "OpenCode agent: %s" choice))))

(defun org-opencode-clear-model-selection ()
  "Clear the currently selected model for this session."
  (interactive)
  (setq-local org-opencode-selected-model nil)
  (message "OpenCode model selection cleared (using default)"))

(defun org-opencode-clear-agent-selection ()
  "Clear the currently selected agent for this session."
  (interactive)
  (setq-local org-opencode-selected-agent nil)
  (message "OpenCode agent selection cleared (using default)"))

(defun org-opencode-show-selections ()
  "Display the currently selected model and agent in the echo area."
  (interactive)
  (message "OpenCode selections - Model: %s, Agent: %s"
           (or org-opencode-selected-model "default")
           (or org-opencode-selected-agent "default")))

;; -----------------------------------------------------------------------------
;; Send as Entry
;; -----------------------------------------------------------------------------

(defun org-opencode-send-as-entry (prompt)
  "Send PROMPT using the structured Org entry layout.
Like `org-opencode-send' but forces `org-opencode-response-layout'
to \\='entry so the exchange is rendered as a proper Org subtree
regardless of the user's default setting."
  (interactive (list (org-opencode--read-prompt current-prefix-arg)))
  (let ((org-opencode-response-layout 'entry))
    (org-opencode-send prompt)))

(provide 'org-opencode-ui)
;;; org-opencode-ui.el ends here