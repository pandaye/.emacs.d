;;; org-opencode-core.el --- Core module for org-opencode -*- lexical-binding: t; -*-

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

;; Core infrastructure for org-opencode: HTTP client, server lifecycle,
;; JSON helpers, URL helpers, and low-level API wrappers.
;;
;; This module extracts all infrastructure code from the original monolithic
;; org-opencode.el into a clean, reusable core.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'subr-x)
(require 'seq)

;; Defined by `url-http.el` at runtime; declare for byte-compiler.
(defvar url-http-end-of-headers)

(defgroup org-opencode nil
  "Use opencode from Org buffers."
  :group 'org
  :prefix "org-opencode-")

(defcustom org-opencode-command "opencode"
  "Command used to invoke the opencode CLI."
  :type 'string
  :group 'org-opencode)

(defcustom org-opencode-server-host "127.0.0.1"
  "Hostname used by the local opencode server."
  :type 'string
  :group 'org-opencode)

(defcustom org-opencode-server-port 4096
  "Port used by the local opencode server."
  :type 'integer
  :group 'org-opencode)

(defcustom org-opencode-server-start-timeout 10
  "Maximum seconds to wait for `opencode serve` to become healthy."
  :type 'number
  :group 'org-opencode)

(defcustom org-opencode-session-title-function #'org-opencode-default-session-title
  "Function used to compute a default session title for the current buffer."
  :type 'function
  :group 'org-opencode)

(defcustom org-opencode-session-directory-function #'org-opencode-default-directory
  "Function used to compute the project directory sent to opencode."
  :type 'function
  :group 'org-opencode)

(defvar org-opencode--server-process nil
  "Process object for the locally managed opencode server.")

(defconst org-opencode--server-buffer-name "*org-opencode-server*"
  "Buffer used for the locally managed opencode server logs.")

(defun org-opencode-default-directory ()
  "Return the default project directory for the current buffer."
  (expand-file-name default-directory))

(defun org-opencode-default-session-title ()
  "Return the default session title for the current buffer."
  (format "Org: %s" (buffer-name)))

(defun org-opencode--executable ()
  "Return the resolved opencode executable path."
  (or (executable-find org-opencode-command)
      (user-error "Cannot find `%s` in `exec-path`" org-opencode-command)))

(defun org-opencode--curl-executable ()
  "Return the resolved curl executable path."
  (or (executable-find "curl")
      (user-error "Cannot find `curl` in `exec-path`; it is required for `/event` streaming")))

(defun org-opencode--base-url ()
  "Return the base URL for the opencode server."
  (format "http://%s:%d" org-opencode-server-host org-opencode-server-port))

(defun org-opencode--session-directory ()
  "Return the directory to send to the opencode API."
  (let ((directory (funcall org-opencode-session-directory-function)))
    (when (and directory (not (string-empty-p directory)))
      (expand-file-name directory))))

(defun org-opencode--session-query ()
  "Return a query alist for requests scoped to the current directory."
  (let ((directory (org-opencode--session-directory)))
    (when directory
      `(("directory" . ,directory)))))

(defun org-opencode--path-with-query (path &optional query)
  "Return PATH with QUERY appended as a URL query string."
  (if (not query)
      path
    (concat
     path
     "?"
     (mapconcat
      (lambda (pair)
        (concat (url-hexify-string (car pair))
                "="
                (url-hexify-string (format "%s" (cdr pair)))))
      query
      "&"))))

(defun org-opencode--json-encode (object)
  "Return OBJECT encoded as UTF-8 JSON."
  (encode-coding-string
   (json-serialize object
                   :false-object :json-false
                   :null-object nil)
   'utf-8))

(defun org-opencode--parse-http-buffer (buffer)
  "Parse an HTTP response from BUFFER and return decoded JSON."
  (with-current-buffer buffer
    (goto-char (point-min))
    (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (error "Malformed HTTP response from opencode"))
    (let ((status-code (string-to-number (match-string 1))))
      (goto-char (or url-http-end-of-headers (point-min)))
      (let ((body (string-trim-left
                   (buffer-substring-no-properties (point) (point-max)))))
        (cond
         ((and (>= status-code 200) (< status-code 300))
          (if (string-empty-p body)
              t
            (json-parse-string body
                               :object-type 'alist
                               :array-type 'list
                               :null-object nil
                               :false-object :json-false)))
         (t
          (error "opencode HTTP %s: %s"
                 status-code
                 (if (string-empty-p body) "empty response" body))))))))

(defun org-opencode--http-json (method path &optional payload)
  "Send METHOD to PATH with optional JSON PAYLOAD and decode the response."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (when payload '(("Content-Type" . "application/json"))))
         (url-request-data (when payload (org-opencode--json-encode payload)))
         (buffer (url-retrieve-synchronously
                  (concat (org-opencode--base-url) path)
                  t t org-opencode-server-start-timeout)))
    (unless buffer
      (error "Failed to contact opencode server"))
    (unwind-protect
        (org-opencode--parse-http-buffer buffer)
      (kill-buffer buffer))))

(defun org-opencode--http-json-async (method path payload callback)
  "Send METHOD to PATH with PAYLOAD and call CALLBACK with result or error.

CALLBACK receives two arguments: RESULT and ERROR. Exactly one of them is
non-nil."
  (let ((url-request-method method)
        (url-request-extra-headers
         (when payload '(("Content-Type" . "application/json"))))
        (url-request-data (when payload (org-opencode--json-encode payload))))
    (url-retrieve
     (concat (org-opencode--base-url) path)
     (lambda (status)
       (let ((transport-error (plist-get status :error)))
         (unwind-protect
             (cond
              (transport-error
               (funcall callback nil (format "%s" transport-error)))
              (t
               (condition-case err
                   (funcall callback (org-opencode--parse-http-buffer (current-buffer)) nil)
                 (error
                  (funcall callback nil (error-message-string err))))))
           (kill-buffer (current-buffer)))))
     nil t t)))

(defun org-opencode--healthy-p ()
  "Return non-nil when the configured opencode server is healthy."
  (condition-case nil
      (let ((reply (org-opencode--http-json "GET" "/global/health")))
        (eq (alist-get 'healthy reply) t))
    (error nil)))

(defun org-opencode--server-live-p ()
  "Return non-nil when the locally managed opencode process is alive."
  (and org-opencode--server-process
       (process-live-p org-opencode--server-process)))

(defun org-opencode--server-command ()
  "Return the command vector used to launch `opencode serve`."
  (list (org-opencode--executable)
        "serve"
        "--hostname" org-opencode-server-host
        "--port" (number-to-string org-opencode-server-port)
        "--print-logs"))

(defun org-opencode-start-server ()
  "Start `opencode serve` unless a healthy server is already available."
  (interactive)
  (cond
   ((org-opencode--healthy-p)
    (message "opencode server is already available at %s"
             (org-opencode--base-url)))
   ((org-opencode--server-live-p)
    (message "Waiting for opencode server to become ready...")
    (org-opencode--wait-for-server))
   (t
    (let ((buffer (get-buffer-create org-opencode--server-buffer-name)))
      (setq org-opencode--server-process
            (make-process
             :name "org-opencode-serve"
             :buffer buffer
             :command (org-opencode--server-command)
             :coding 'utf-8
             :connection-type 'pipe
             :noquery t
             :sentinel #'org-opencode--server-sentinel))
      (org-opencode--wait-for-server)
      (message "Started opencode server at %s" (org-opencode--base-url))))))

(defun org-opencode--wait-for-server ()
  "Wait until the configured opencode server becomes healthy."
  (let ((deadline (+ (float-time) org-opencode-server-start-timeout)))
    (while (and (< (float-time) deadline)
                (not (org-opencode--healthy-p)))
      (accept-process-output org-opencode--server-process 0.1)
      (sleep-for 0.1))
    (unless (org-opencode--healthy-p)
      (error "Timed out waiting for opencode server at %s"
             (org-opencode--base-url)))))

(defun org-opencode-stop-server ()
  "Stop the locally managed opencode server."
  (interactive)
  (unless (org-opencode--server-live-p)
    (user-error "No locally managed opencode server is running"))
  (delete-process org-opencode--server-process)
  (setq org-opencode--server-process nil)
  (message "Stopped org-opencode server"))

(defun org-opencode--server-sentinel (process _event)
  "Handle state changes for the opencode server PROCESS."
  (unless (process-live-p process)
    (when (eq process org-opencode--server-process)
      (setq org-opencode--server-process nil))
    (message "org-opencode server exited")))

(defun org-opencode--ensure-server ()
  "Ensure that a healthy opencode server is available."
  (unless (org-opencode--healthy-p)
    (org-opencode-start-server)))

;; API convenience wrappers

(defun org-opencode-api-sessions (&optional query)
  "List all sessions with optional QUERY alist.
QUERY can include filters like `((\"directory\" . \"/path\"))'.
Returns a list of session alists."
  (org-opencode--http-json "GET" (org-opencode--path-with-query "/session" query)))

(defun org-opencode-api-session (session-id)
  "Get a single session by SESSION-ID.
Returns a session alist with id, title, created_at, etc."
  (org-opencode--http-json "GET" (format "/session/%s" session-id)))

(defun org-opencode-api-delete-session (session-id)
  "Delete a session by SESSION-ID.
Returns t on success."
  (org-opencode--http-json "DELETE" (format "/session/%s" session-id)))

(defun org-opencode-api-messages (session-id)
  "List all messages for SESSION-ID.
Returns a list of message alists."
  (org-opencode--http-json "GET" (format "/session/%s/message" session-id)))

(defun org-opencode-api-message (session-id message-id)
  "Get a single MESSAGE-ID from SESSION-ID.
Returns a message alist."
  (org-opencode--http-json "GET" (format "/session/%s/message/%s" session-id message-id)))

(defun org-opencode-api-providers ()
  "List all available providers.
Returns a list of provider alists."
  (org-opencode--http-json "GET" "/provider"))

(defun org-opencode-api-agents ()
  "List all available agents.
Returns a list of agent alists."
  (org-opencode--http-json "GET" "/agent"))

(defun org-opencode-api-session-diff (session-id)
  "Get the pending diff for SESSION-ID.
Returns an alist with diff information."
  (org-opencode--http-json "GET" (format "/session/%s/diff" session-id)))

(defun org-opencode-api-approve-permission (session-id permission-id response)
  "Approve or reject a permission request.
SESSION-ID is the session containing the permission.
PERMISSION-ID is the permission to respond to.
RESPONSE should be one of: \"once\", \"always\", or \"reject\".
Returns t on success."
  (org-opencode--http-json
   "POST"
   (format "/session/%s/permissions/%s" session-id permission-id)
   `((response . ,response))))

(defun org-opencode-api-fork-session (session-id)
  "Fork SESSION-ID into a new session.
Returns a new session alist with the forked session's details."
  (org-opencode--http-json "POST" (format "/session/%s/fork" session-id)))

(defun org-opencode-api-revert (session-id)
  "Revert the last assistant message in SESSION-ID.
Returns the updated session state."
  (org-opencode--http-json "POST" (format "/session/%s/revert" session-id)))

(defun org-opencode-api-unrevert (session-id)
  "Undo the last revert in SESSION-ID, restoring the reverted message.
Returns the updated session state."
  (org-opencode--http-json "POST" (format "/session/%s/unrevert" session-id)))

(defun org-opencode-api-command (session-id command)
  "Execute slash COMMAND in SESSION-ID.
COMMAND is a string like \"compact\" or \"plan\".
Returns the command execution result."
  (org-opencode--http-json
   "POST"
   (format "/session/%s/command" session-id)
   `((command . ,command))))

(defun org-opencode-api-summarize (session-id)
  "Request a summary of SESSION-ID.
Returns the summary data."
  (org-opencode--http-json "POST" (format "/session/%s/summarize" session-id)))

(defun org-opencode-api-todo (session-id)
  "Get the todo list for SESSION-ID.
Returns a list of todo item alists."
  (org-opencode--http-json "GET" (format "/session/%s/todo" session-id)))

(provide 'org-opencode-core)
;;; org-opencode-core.el ends here
