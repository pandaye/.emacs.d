;;; org-opencode.el --- Org frontend for opencode -*- lexical-binding: t; -*-

;; Author: pandaye
;; Keywords: outlines, ai, tools

;;; Commentary:

;; This file implements the first backend-correct version of an Org frontend
;; for opencode. It talks to the official stateful `opencode serve` HTTP API
;; instead of the one-shot `opencode run` CLI, which keeps the design aligned
;; with a future full frontend that can replace the TUI.

;;; Code:

(require 'json)
(require 'org)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

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

(defcustom org-opencode-render-nontext-parts t
  "When non-nil, include simple markers for non-text assistant parts."
  :type 'boolean
  :group 'org-opencode)

(defcustom org-opencode-after-response-hook nil
  "Hook run after an opencode response is inserted."
  :type 'hook
  :group 'org-opencode)

(defcustom org-opencode-response-layout 'src-block
  "Layout used when inserting opencode exchanges.

When set to `src-block', insert only a response block at point.
When set to `entry', create a structured Org subtree containing prompt
and response sections."
  :type '(choice (const :tag "Plain src block" src-block)
                 (const :tag "Structured entry" entry))
  :group 'org-opencode)

(defcustom org-opencode-store-prompt-in-entry t
  "When non-nil, include the user prompt in structured entry layout."
  :type 'boolean
  :group 'org-opencode)

(defcustom org-opencode-entry-heading-prefix "OpenCode"
  "Heading prefix used for structured entry layout."
  :type 'string
  :group 'org-opencode)

(defcustom org-opencode-session-id-property "OPENCODE_SESSION_ID"
  "Org property name used to store opencode session id in entries."
  :type 'string
  :group 'org-opencode)

(defcustom org-opencode-file-session-keyword "OPENCODE_SESSION_ID"
  "File keyword used to persist the current opencode session id.

When auto session mode is enabled, this keyword is read when
`org-opencode-mode' turns on, and updated when a new session is created."
  :type 'string
  :group 'org-opencode)

(defcustom org-opencode-auto-session-on-mode-enable t
  "When non-nil, auto-load or auto-create a session on mode enable.

If `org-opencode-file-session-keyword' exists in the current Org file,
its session id is adopted. Otherwise, a new session is created and written
back to the file keyword."
  :type 'boolean
  :group 'org-opencode)

(defcustom org-opencode-send-headline-by-default t
  "When non-nil, `org-opencode-send' uses current headline as default prompt.

Prompt source priority is: active region, current headline content, then
minibuffer. Use prefix argument to force minibuffer input."
  :type 'boolean
  :group 'org-opencode)

(defvar org-opencode--server-process nil
  "Process object for the locally managed opencode server.")

(defconst org-opencode--server-buffer-name "*org-opencode-server*"
  "Buffer used for the locally managed opencode server logs.")

(defvar-local org-opencode--session nil
  "Buffer-local session object returned by the opencode HTTP API.")

(defvar org-opencode--event-process nil
  "Process object for the shared opencode SSE event stream.")

(defconst org-opencode--event-buffer-name "*org-opencode-events*"
  "Buffer used for opencode SSE event logs and debugging.")

(defvar org-opencode--pending-streams (make-hash-table :test 'equal)
  "Map opencode session ids to active render states.")

(defvar org-opencode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x C-v") #'org-opencode-send)
    (define-key map (kbd "C-c C-x C-s") #'org-opencode-new-session)
    (define-key map (kbd "C-c C-x C-a") #'org-opencode-abort)
    map)
  "Keymap for `org-opencode-mode'.")

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
  (when (org-opencode--event-process-live-p)
    (org-opencode-stop-event-stream))
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

(defun org-opencode--session-id ()
  "Return the current buffer session id, or nil."
  (alist-get 'id org-opencode--session))

(defun org-opencode--file-keyword-value (keyword)
  "Return first file-level KEYWORD value from current Org buffer."
  (let* ((key (upcase keyword))
         (alist (org-collect-keywords (list key))))
    (car (cdr (assoc key alist)))))

(defun org-opencode--set-file-keyword (keyword value)
  "Set file-level KEYWORD to VALUE in current Org buffer."
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

(defun org-opencode--remember-session-in-file (&optional session-id)
  "Persist SESSION-ID to file keyword in current Org buffer."
  (let ((sid (or session-id (org-opencode--session-id))))
    (when (and sid (not (string-empty-p sid)) (derived-mode-p 'org-mode))
      (org-opencode--set-file-keyword org-opencode-file-session-keyword sid))))

(defun org-opencode--adopt-session-id (session-id)
  "Adopt SESSION-ID into current buffer-local opencode state."
  (setq org-opencode--session `((id . ,session-id)))
  session-id)

(defun org-opencode--load-session-from-file ()
  "Load session id from file keyword and adopt it.

Return adopted session id, or nil when absent."
  (let ((sid (org-opencode--file-keyword-value org-opencode-file-session-keyword)))
    (when (and sid (not (string-empty-p sid)))
      (org-opencode--adopt-session-id sid))))

(defun org-opencode--maybe-auto-session ()
  "Auto-load or auto-create session for current Org buffer when enabled."
  (when (and org-opencode-auto-session-on-mode-enable
             (derived-mode-p 'org-mode)
             (not (org-opencode--session-id)))
    (or (org-opencode--load-session-from-file)
        (progn
          (org-opencode-new-session (funcall org-opencode-session-title-function))
          (org-opencode--remember-session-in-file))))
  org-opencode--session)

(defun org-opencode--make-render-state (session-id marker)
  "Create a render state for SESSION-ID at MARKER."
  (let ((state (make-hash-table :test 'eq)))
    (puthash :session-id session-id state)
    (puthash :buffer (marker-buffer marker) state)
    (puthash :content-start (copy-marker marker nil) state)
    (puthash :content-end nil state)
    (puthash :assistant-message-id nil state)
    (puthash :part-order nil state)
    (puthash :parts (make-hash-table :test 'equal) state)
    state))

(defun org-opencode--pending-state (session-id)
  "Return the active render state for SESSION-ID, or nil."
  (gethash session-id org-opencode--pending-streams))

(defun org-opencode--state-buffer-live-p (state)
  "Return non-nil when STATE still points to a live buffer."
  (buffer-live-p (gethash :buffer state)))

(defun org-opencode--set-part (state part)
  "Store PART in STATE and preserve arrival order."
  (let* ((part-id (alist-get 'id part))
         (parts (gethash :parts state))
         (order (gethash :part-order state)))
    (unless (gethash part-id parts)
      (puthash :part-order (append order (list part-id)) state))
    (puthash part-id part parts)))

(defun org-opencode--part-text (part)
  "Render PART into display text, or nil if it should be hidden."
  (pcase (alist-get 'type part)
    ("text" (alist-get 'text part))
    ((or "step-start" "step-finish") nil)
    ((pred (lambda (_type) org-opencode-render-nontext-parts))
     (format "[%s]" (alist-get 'type part)))
    (_ nil)))

(defun org-opencode--state-render-text (state)
  "Render the current STATE into a markdown string."
  (let ((parts (gethash :parts state)))
    (string-join
     (delq nil
           (mapcar (lambda (part-id)
                     (org-opencode--part-text (gethash part-id parts)))
                   (gethash :part-order state)))
     "\n\n")))

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
              (set-marker end (point)))))))))

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

(defun org-opencode--prepare-response-marker (prompt session-id)
  "Return an insertion marker for PROMPT in SESSION-ID.

Marker placement depends on `org-opencode-response-layout'."
  (pcase org-opencode-response-layout
    ('entry
     (org-opencode--insert-exchange-shell prompt session-id))
    (_
     (copy-marker (point) t))))

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
    (puthash session-id state org-opencode--pending-streams)
    state))

(defun org-opencode--finish-render-state (session-id)
  "Forget the active render state for SESSION-ID."
  (let ((state (org-opencode--pending-state session-id)))
    (when state
      (let ((start (gethash :content-start state))
            (end (gethash :content-end state)))
        (when start
          (set-marker start nil))
        (when end
          (set-marker end nil))))
    (remhash session-id org-opencode--pending-streams)))

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

(defun org-opencode--event-process-live-p ()
  "Return non-nil when the shared SSE event process is alive."
  (and org-opencode--event-process
       (process-live-p org-opencode--event-process)))

(defun org-opencode--event-url ()
  "Return the SSE endpoint URL."
  (concat (org-opencode--base-url) "/event"))

(defun org-opencode-start-event-stream ()
  "Start the shared `/event` subscription if needed."
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
      (process-put org-opencode--event-process :remainder "")
      (process-put org-opencode--event-process :data-lines nil)
      (message "Subscribed to opencode /event stream"))))

(defun org-opencode-stop-event-stream ()
  "Stop the shared `/event` subscription."
  (interactive)
  (unless (org-opencode--event-process-live-p)
    (user-error "No opencode event stream is active"))
  (process-put org-opencode--event-process :stopping t)
  (delete-process org-opencode--event-process)
  (setq org-opencode--event-process nil)
  (message "Stopped opencode /event stream"))

(defun org-opencode--event-sentinel (process _event)
  "Handle lifecycle changes for the SSE PROCESS."
  (unless (process-live-p process)
    (when (eq process org-opencode--event-process)
      (setq org-opencode--event-process nil))
    (unless (or (process-get process :stopping)
                (memq (process-exit-status process) '(0 15)))
      (message "org-opencode event stream exited unexpectedly"))))

(defun org-opencode--event-filter (process chunk)
  "Handle SSE CHUNK from PROCESS."
  (let ((input (concat (or (process-get process :remainder) "") chunk))
        line)
    (while (string-match "\n" input)
      (setq line (substring input 0 (match-beginning 0))
            input (substring input (match-end 0)))
      (org-opencode--event-line process (string-remove-suffix "\r" line)))
    (process-put process :remainder input)))

(defun org-opencode--event-line (process line)
  "Handle one SSE LINE from PROCESS."
  (if (string-empty-p line)
      (let ((data-lines (process-get process :data-lines)))
        (when data-lines
          (process-put process :data-lines nil)
          (org-opencode--dispatch-event
           (string-join (nreverse data-lines) "\n"))))
    (when (string-prefix-p "data:" line)
      (process-put process :data-lines
                   (cons (string-trim-left (substring line 5))
                         (process-get process :data-lines))))))

(defun org-opencode--dispatch-event (payload)
  "Dispatch one SSE PAYLOAD."
  (condition-case err
      (let* ((event (json-parse-string payload
                                       :object-type 'alist
                                       :array-type 'list
                                       :null-object nil
                                       :false-object :json-false))
             (type (alist-get 'type event))
             (properties (alist-get 'properties event)))
        (pcase type
          ("message.updated"
           (org-opencode--event-message-updated properties))
          ("message.part.updated"
           (org-opencode--event-part-updated properties))
          ("message.part.delta"
           (org-opencode--event-part-delta properties))
          (_ nil)))
    (error
     (message "Failed to parse opencode event: %s" (error-message-string err)))))

(defun org-opencode--event-message-updated (properties)
  "Handle `message.updated` PROPERTIES."
  (let* ((info (alist-get 'info properties))
         (session-id (alist-get 'sessionID info))
         (state (org-opencode--pending-state session-id)))
    (when (and state (equal (alist-get 'role info) "assistant"))
      (puthash :assistant-message-id (alist-get 'id info) state))))

(defun org-opencode--event-part-updated (properties)
  "Handle `message.part.updated` PROPERTIES."
  (let* ((part (alist-get 'part properties))
         (session-id (alist-get 'sessionID part))
         (state (org-opencode--pending-state session-id))
         (message-id (alist-get 'messageID part)))
    (when (and state
               (gethash :assistant-message-id state)
               (equal (gethash :assistant-message-id state) message-id))
      (org-opencode--set-part state part)
      (org-opencode--refresh-render-state state))))

(defun org-opencode--event-part-delta (properties)
  "Handle `message.part.delta` PROPERTIES."
  (let* ((session-id (alist-get 'sessionID properties))
         (state (org-opencode--pending-state session-id))
         (message-id (alist-get 'messageID properties))
         (part-id (alist-get 'partID properties))
         (field (alist-get 'field properties))
         (delta (alist-get 'delta properties)))
    (when (and state
               (equal field "text")
               (gethash :assistant-message-id state)
               (equal (gethash :assistant-message-id state) message-id))
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
        (org-opencode--refresh-render-state state)))))

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
        (string-trim (read-string "Prompt for opencode: ")))))

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

(defun org-opencode--escape-src-fences (text)
  "Escape Org src block delimiters in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward
            "^[[:space:]]*#\\+\\(begin\\|end\\)_src\\b" nil t)
      (replace-match ",\\&" t t))
    (buffer-string)))

(defun org-opencode--insert-response (marker output)
  "Insert OUTPUT at MARKER as a markdown src block."
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
  "Render PARTS into markdown text for Org insertion."
  (string-join
   (delq
    nil
    (mapcar
     (lambda (part)
       (pcase (alist-get 'type part)
        ("text" (alist-get 'text part))
        ((or "step-start" "step-finish") nil)
        ((pred (lambda (_type) org-opencode-render-nontext-parts))
         (format "[%s]" (alist-get 'type part)))
        (_ nil)))
     parts))
   "\n\n"))

(defun org-opencode-new-session (&optional title)
  "Create a new opencode session for the current Org buffer."
  (interactive
   (list
    (let ((default (funcall org-opencode-session-title-function)))
      (read-string "Session title: " default nil default))))
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-opencode-new-session` only works in Org buffers"))
  (org-opencode--ensure-server)
  (setq org-opencode--session
        (org-opencode--http-json
         "POST"
         (org-opencode--path-with-query "/session" (org-opencode--session-query))
         `((title . ,title))))
  (org-opencode--remember-session-in-file)
  (message "Created opencode session %s" (org-opencode--session-id)))

(defun org-opencode--ensure-session ()
  "Ensure the current Org buffer has an opencode session."
  (unless (org-opencode--session-id)
    (org-opencode-new-session (funcall org-opencode-session-title-function)))
  org-opencode--session)

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
  (message "Aborted opencode session %s" (org-opencode--session-id)))

(defun org-opencode-adopt-session-from-heading ()
  "Adopt the session id stored at current Org heading.

This allows resuming a previous opencode conversation from archived
entries that contain `org-opencode-session-id-property'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-opencode-adopt-session-from-heading` only works in Org buffers"))
  (let ((session-id (org-entry-get nil org-opencode-session-id-property t)))
    (unless (and session-id (not (string-empty-p session-id)))
      (user-error "No %s property found at current heading" org-opencode-session-id-property))
    (org-opencode--adopt-session-id session-id)
    (message "Adopted opencode session %s" session-id)))

(defun org-opencode-reset-session ()
  "Forget the current buffer-local opencode session."
  (interactive)
  (setq org-opencode--session nil)
  (message "Cleared current opencode session"))

(defun org-opencode-send (prompt)
  "Send PROMPT to the current opencode session and insert the reply."
  (interactive (list (org-opencode--read-prompt current-prefix-arg)))
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-opencode-send` only works in Org buffers"))
  (when (string-empty-p prompt)
    (user-error "Prompt is empty"))
  (org-opencode--ensure-server)
  (org-opencode--ensure-session)
  (let* ((session-id (org-opencode--session-id))
         (path (org-opencode--path-with-query
                (format "/session/%s/message" session-id)
                (org-opencode--session-query)))
         (payload `((parts . [((type . "text") (text . ,prompt))])))
         (marker nil)
         (state nil))
    (when (org-opencode--pending-state session-id)
      (user-error "Session %s already has a pending streamed response" session-id))
    (org-opencode-start-event-stream)
    (setq marker (org-opencode--prepare-response-marker prompt session-id))
    (setq state (org-opencode--start-render-state session-id marker))
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
         (message "opencode request failed: %s" error-message))
        (t
         (org-opencode--merge-final-reply state reply)
         (run-hooks 'org-opencode-after-response-hook)
         (message "Inserted streamed opencode response")))
       (org-opencode--finish-render-state session-id)))))

(defun org-opencode-send-as-entry (prompt)
  "Send PROMPT and force insertion as a structured Org entry."
  (interactive (list (org-opencode--read-prompt current-prefix-arg)))
  (let ((org-opencode-response-layout 'entry))
    (org-opencode-send prompt)))

;;;###autoload
(define-minor-mode org-opencode-mode
  "Minor mode for talking to the official opencode backend from Org buffers."
  :lighter " OpenCode"
  :keymap org-opencode-mode-map
  (when org-opencode-mode
    (org-opencode--maybe-auto-session)))

(define-key org-opencode-mode-map (kbd "C-c C-x C-e") #'org-opencode-send-as-entry)
(define-key org-opencode-mode-map (kbd "C-c C-x C-r") #'org-opencode-adopt-session-from-heading)

(provide 'org-opencode)
;;; org-opencode.el ends here
