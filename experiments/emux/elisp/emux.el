;;; emux.el --- Control emux from managed Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal IPC client for the emux TUI compositor experiment.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'tabulated-list)

(defgroup emux nil
  "Control an outer emux TUI compositor."
  :group 'tools
  :prefix "emux-")

(defcustom emux-socket-path
  (or (getenv "EMUX_SOCKET")
      (format "/tmp/emux-%s.sock" (user-uid)))
  "Unix socket path used by emux."
  :type 'file
  :group 'emux)

(defcustom emux-manager-refresh-interval 1.0
  "Seconds between automatic refreshes of `emux-manager-mode' buffers."
  :type 'number
  :group 'emux)

(defconst emux-manager-buffer-name "*emux-manager*"
  "Buffer name for emux window manager.")

(defvar-local emux-manager--marks nil
  "Hash table of marked emux windows in manager buffer.")

(defvar-local emux-manager--refresh-timer nil
  "Automatic refresh timer for current emux manager buffer.")

(defun emux--send-command (command)
  "Send COMMAND to the outer emux process and return its response."
  (unless (file-exists-p emux-socket-path)
    (user-error "emux socket does not exist: %s" emux-socket-path))
  (let ((response "")
        (done nil))
    (let ((proc (make-network-process
                 :name "emux-ipc"
                 :family 'local
                 :service emux-socket-path
                 :coding 'utf-8
                 :noquery t
                 :filter (lambda (_proc string)
                           (setq response (concat response string)))
                 :sentinel (lambda (_proc _event)
                             (setq done t)))))
    (process-send-string proc (concat command "\n"))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output proc 0.05)))
      (when (process-live-p proc)
        (delete-process proc))
      response)))

(defun emux-open-shell ()
  "Open a shell surface in the outer emux compositor."
  (interactive)
  (emux--send-command "open-shell"))

(defun emux-focus-next ()
  "Focus the next emux surface."
  (interactive)
  (emux--send-command "focus-next"))

(defun emux-close-focused ()
  "Close the focused emux surface."
  (interactive)
  (emux--send-command "close-focused"))

(defun emux-open-manager ()
  "Ask outer emux to focus Emacs and open `emux-manager'."
  (interactive)
  (let ((response (emux--send-command "open-manager")))
    (unless (string-prefix-p "OK" response)
      (user-error "%s" (string-trim response)))))

(defun emux-send-region (beg end &optional surface-id)
  "Send region from BEG to END to SURFACE-ID.
When SURFACE-ID is omitted, use surface 1, which is the first shell opened by
the default MVP layout."
  (interactive "r")
  (let ((id (or surface-id 1))
        (text (string-trim-right (buffer-substring-no-properties beg end))))
    (emux--send-command (format "send %s %s" id text))))

(defun emux-list-windows ()
  "Return emux windows as a list of plists."
  (let ((response (string-trim-right (emux--send-command "list-windows"))))
    (if (string-empty-p response)
        nil
      (mapcar (lambda (line)
                (let ((parts (split-string line "\t")))
                  (list :id (string-to-number (or (nth 0 parts) "0"))
                        :active (string= (nth 1 parts) "1")
                        :pid (or (nth 2 parts) "")
                        :process (or (nth 3 parts) "")
                        :title (or (nth 4 parts) ""))))
              (split-string response "\n" t)))))

(defun emux-focus-window (id)
  "Focus emux window ID."
  (interactive (list (read-number "Window id: ")))
  (let ((response (emux--send-command (format "focus-window %s" id))))
    (unless (string-prefix-p "OK" response)
      (user-error "%s" (string-trim response)))))

(defun emux-close-window (id)
  "Close emux window ID."
  (interactive (list (read-number "Window id: ")))
  (let ((response (emux--send-command (format "close-window %s" id))))
    (unless (string-prefix-p "OK" response)
      (user-error "%s" (string-trim response)))))

(defun emux-rename-window (id name)
  "Rename emux window ID to NAME."
  (interactive (list (read-number "Window id: ")
                     (read-string "New name: ")))
  (let ((response (emux--send-command (format "rename-window %s %s" id name))))
    (unless (string-prefix-p "OK" response)
      (user-error "%s" (string-trim response)))))

(defun emux-new-window (command)
  "Create a new emux window running COMMAND."
  (interactive (list (read-string "Command: " (or (getenv "SHELL") "/bin/sh"))))
  (let ((response (emux--send-command (format "new-window %s" command))))
    (unless (string-prefix-p "OK" response)
      (user-error "%s" (string-trim response)))))

(defvar emux-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'emux-manager-refresh)
    (define-key map (kbd "RET") #'emux-manager-visit)
    (define-key map (kbd "j") #'emux-manager-visit)
    (define-key map (kbd "n") #'emux-manager-new-shell)
    (define-key map (kbd "r") #'emux-manager-rename)
    (define-key map (kbd "d") #'emux-manager-mark-delete)
    (define-key map (kbd "u") #'emux-manager-unmark)
    (define-key map (kbd "x") #'emux-manager-execute-deletions)
    map)
  "Keymap for `emux-manager-mode'.")

(define-derived-mode emux-manager-mode tabulated-list-mode "Emux-Manager"
  "Major mode for managing emux windows."
  (setq tabulated-list-format [(" " 1 nil)
                               ("State" 8 t)
                               ("ID" 8 t)
                               ("PID" 8 t)
                               ("Process" 16 t)
                               ("Title" 60 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "ID" nil))
  (unless (hash-table-p emux-manager--marks)
    (setq emux-manager--marks (make-hash-table :test 'equal)))
  (emux-manager--start-auto-refresh)
  (add-hook 'kill-buffer-hook #'emux-manager--stop-auto-refresh nil t)
  (tabulated-list-init-header))

(defun emux-manager--start-auto-refresh ()
  "Start automatic refresh for current manager buffer."
  (emux-manager--stop-auto-refresh)
  (let ((buffer (current-buffer)))
    (setq emux-manager--refresh-timer
          (run-at-time emux-manager-refresh-interval
                       emux-manager-refresh-interval
                       (lambda ()
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (when (derived-mode-p 'emux-manager-mode)
                               (ignore-errors
                                 (emux-manager-refresh))))))))))

(defun emux-manager--stop-auto-refresh ()
  "Stop automatic refresh for current manager buffer."
  (when (timerp emux-manager--refresh-timer)
    (cancel-timer emux-manager--refresh-timer))
  (setq emux-manager--refresh-timer nil))

(defun emux-manager--entry (window)
  "Build tabulated-list entry for WINDOW."
  (let* ((id (plist-get window :id))
         (id-string (number-to-string id))
         (marked (if (gethash id emux-manager--marks) "D" ""))
         (state (if (plist-get window :active) "active" "")))
    (list id
          (vector marked
                  state
                  id-string
                  (plist-get window :pid)
                  (plist-get window :process)
                  (plist-get window :title)))))

(defun emux-manager-refresh ()
  "Refresh emux manager buffer."
  (interactive)
  (let ((id-at-point (tabulated-list-get-id)))
    (setq tabulated-list-entries (mapcar #'emux-manager--entry (emux-list-windows)))
    (tabulated-list-print t)
    (when id-at-point
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) id-at-point)))
        (forward-line 1)))))

(defun emux-manager--id-at-point ()
  "Return emux window id at point."
  (or (tabulated-list-get-id)
      (user-error "No emux window on this line")))

(defun emux-manager-visit ()
  "Focus the emux window at point."
  (interactive)
  (emux-focus-window (emux-manager--id-at-point))
  (emux-manager-refresh))

(defun emux-manager-new-shell ()
  "Create a new shell window."
  (interactive)
  (emux-open-shell)
  (emux-manager-refresh))

(defun emux-manager-rename ()
  "Rename the emux window at point."
  (interactive)
  (let* ((id (emux-manager--id-at-point))
         (window (seq-find (lambda (window) (= id (plist-get window :id)))
                           (emux-list-windows)))
         (name (read-string "New name: " (plist-get window :title))))
    (emux-rename-window id name)
    (emux-manager-refresh)))

(defun emux-manager-mark-delete ()
  "Mark the emux window at point for deletion."
  (interactive)
  (puthash (emux-manager--id-at-point) t emux-manager--marks)
  (emux-manager-refresh))

(defun emux-manager-unmark ()
  "Unmark the emux window at point."
  (interactive)
  (remhash (emux-manager--id-at-point) emux-manager--marks)
  (emux-manager-refresh))

(defun emux-manager--marked-ids ()
  "Return marked emux window ids."
  (let (ids)
    (maphash (lambda (id marked)
               (when marked
                 (push id ids)))
             emux-manager--marks)
    (nreverse ids)))

(defun emux-manager-execute-deletions ()
  "Delete marked emux windows."
  (interactive)
  (let ((ids (emux-manager--marked-ids)))
    (unless ids
      (user-error "No marked windows"))
    (when (yes-or-no-p (format "Close %d emux window(s)? " (length ids)))
      (dolist (id ids)
        (emux-close-window id)
        (remhash id emux-manager--marks))
      (emux-manager-refresh))))

(defun emux-manager ()
  "Open the emux window manager."
  (interactive)
  (let ((buffer (get-buffer-create emux-manager-buffer-name)))
    (with-current-buffer buffer
      (emux-manager-mode)
      (emux-manager-refresh))
    (pop-to-buffer buffer)))

(provide 'emux)

;;; emux.el ends here
