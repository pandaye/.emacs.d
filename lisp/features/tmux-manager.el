;; -*- lexical-binding: t; -*-
;;; tmux-manager.el --- Dired/ibuffer-style tmux window manager

;; Author: Pandaye
;; Keywords: tmux, convenience

;;; Commentary:
;; Standalone tmux manager buffer with sync, jump, rename, and bulk delete.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup tmux-manager nil
  "Standalone manager for tmux windows."
  :group 'tools
  :prefix "tmux-manager-")

(defcustom tmux-manager-default-session nil
  "Default tmux session to use when not specified.
If nil, use the currently attached session."
  :type '(choice (const :tag "Current attached session" nil)
                 (string :tag "Session name"))
  :group 'tmux-manager)

(defcustom tmux-manager-window-format
  "#{session_name}\t#{window_index}\t#{window_name}\t#{window_flags}\t#{pane_title}\t#{window_active}"
  "Format string for listing tmux windows.
Columns: session, index, name, flags, title, active."
  :type 'string
  :group 'tmux-manager)

(defconst tmux-manager-buffer-name "*tmux-manager*"
  "Buffer name for tmux manager mode.")

(defvar-local tmux-manager--marks nil
  "Hash table of marked tmux windows in manager buffer.")

(defun tmux-manager--tmux-available-p ()
  "Check if tmux is available and running."
  (and (executable-find "tmux")
       (= 0 (call-process "tmux" nil nil nil "list-sessions"))))

(defun tmux-manager--run-tmux-command (&rest args)
  "Run tmux command with ARGS and return output as string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "tmux" nil t nil args)))
      (if (= exit-code 0)
          (string-trim (buffer-string))
        nil))))

(defun tmux-manager--get-current-session ()
  "Get the currently attached tmux session name."
  (or tmux-manager-default-session
      (tmux-manager--run-tmux-command "display-message" "-p" "#{session_name}")))

(defun tmux-manager--window-target (window)
  "Build tmux target from WINDOW plist."
  (format "%s:%d"
          (plist-get window :session)
          (plist-get window :index)))

(defun tmux-manager--follow (target &optional _prefix-arg)
  "Switch tmux client to TARGET window."
  (unless (tmux-manager--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((parts (split-string target ":"))
         (session (car parts)))
    (unless (string= session (tmux-manager--get-current-session))
      (tmux-manager--run-tmux-command "switch-client" "-t" session))
    (tmux-manager--run-tmux-command "select-window" "-t" target)
    (message "Switched to tmux %s" target)))

(defun tmux-manager--list-windows (&optional session)
  "Return window plists for SESSION or all sessions if SESSION is nil."
  (when (tmux-manager--tmux-available-p)
    (let* ((args (if session
                     (list "list-windows" "-t" session "-F" tmux-manager-window-format)
                   (list "list-windows" "-a" "-F" tmux-manager-window-format)))
           (output (apply #'tmux-manager--run-tmux-command args)))
      (when output
        (mapcar (lambda (line)
                  (let ((parts (split-string line "\t" nil)))
                    (list :session (nth 0 parts)
                          :index (string-to-number (nth 1 parts))
                          :name (or (nth 2 parts) "")
                          :flags (or (nth 3 parts) "")
                          :title (or (nth 4 parts) "")
                          :active (string= (nth 5 parts) "1"))))
                (split-string output "\n" t))))))

(defvar tmux-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'tmux-manager-refresh)
    (define-key map (kbd "RET") #'tmux-manager-visit)
    (define-key map (kbd "j") #'tmux-manager-visit)
    (define-key map (kbd "r") #'tmux-manager-rename)
    (define-key map (kbd "d") #'tmux-manager-mark-delete)
    (define-key map (kbd "u") #'tmux-manager-unmark)
    (define-key map (kbd "x") #'tmux-manager-execute-deletions)
    map)
  "Keymap for `tmux-manager-mode'.")

(define-derived-mode tmux-manager-mode tabulated-list-mode "Tmux-Manager"
  "Major mode for managing tmux windows like dired/ibuffer."
  (setq tabulated-list-format [(" " 1 nil)
                               ("State" 8 t)
                               ("Window" 72 t)
                               ("Index" 8 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Window" nil))
  (unless (hash-table-p tmux-manager--marks)
    (setq tmux-manager--marks (make-hash-table :test 'equal)))
  (tabulated-list-init-header))

(defun tmux-manager--entry (window)
  "Build tabulated list entry from WINDOW plist."
  (let* ((target (tmux-manager--window-target window))
         (marked (if (gethash target tmux-manager--marks) "D" ""))
         (flags (or (plist-get window :flags) ""))
         (state (string-trim flags))
         (window-text (string-trim
                       (format "%s:%s %s"
                               (plist-get window :session)
                               (plist-get window :name)
                               (plist-get window :title)))))
    (list target
          (vector marked
                  state
                  window-text
                  (number-to-string (plist-get window :index))))))

(defun tmux-manager--window-at-point ()
  "Return tmux window plist for current line."
  (let ((target (tabulated-list-get-id)))
    (seq-find (lambda (window)
                (string= target (tmux-manager--window-target window)))
              (tmux-manager--list-windows))))

(defun tmux-manager--marked-targets ()
  "Return marked tmux targets from current manager buffer."
  (let (targets)
    (dolist (entry tabulated-list-entries)
      (let ((target (car entry)))
        (when (gethash target tmux-manager--marks)
          (push target targets))))
    (nreverse targets)))

(defun tmux-manager-refresh ()
  "Synchronize manager buffer with current tmux windows."
  (interactive)
  (unless (tmux-manager--tmux-available-p)
    (user-error "tmux is not running"))
  (let ((target-at-point (tabulated-list-get-id))
        (windows (tmux-manager--list-windows)))
    (setq tabulated-list-entries (mapcar #'tmux-manager--entry windows))
    (tabulated-list-print t)
    (when target-at-point
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) target-at-point)))
        (forward-line 1)))))

(defun tmux-manager-mark-delete ()
  "Mark the tmux window at point for deletion."
  (interactive)
  (let* ((target (tabulated-list-get-id))
         (next-target (save-excursion
                        (forward-line 1)
                        (tabulated-list-get-id))))
    (unless target
      (user-error "No tmux window on this line"))
    (puthash target t tmux-manager--marks)
    (tmux-manager-refresh)
    (when next-target
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) next-target)))
        (forward-line 1)))))

(defun tmux-manager-unmark ()
  "Unmark the tmux window at point."
  (interactive)
  (let ((target (tabulated-list-get-id)))
    (unless target
      (user-error "No tmux window on this line"))
    (remhash target tmux-manager--marks)
    (tmux-manager-refresh)))

(defun tmux-manager-visit ()
  "Jump to the tmux window on current line."
  (interactive)
  (let ((target (tabulated-list-get-id)))
    (unless target
      (user-error "No tmux window on this line"))
    (tmux-manager--follow target)))

(defun tmux-manager-rename ()
  "Rename tmux window on current line."
  (interactive)
  (let* ((window (tmux-manager--window-at-point))
         (target (and window (tmux-manager--window-target window))))
    (unless (and window target)
      (user-error "No tmux window on this line"))
    (let ((new-name (read-string "New name: " (plist-get window :name))))
      (tmux-manager--run-tmux-command "rename-window" "-t" target new-name)
      (tmux-manager-refresh)
      (message "Renamed window %s to %s" target new-name))))

(defun tmux-manager-execute-deletions ()
  "Delete all marked tmux windows in current manager buffer."
  (interactive)
  (let ((targets (tmux-manager--marked-targets)))
    (unless targets
      (user-error "No marked windows"))
    (when (yes-or-no-p (format "Kill %d tmux window(s)? " (length targets)))
      (dolist (target targets)
        (tmux-manager--run-tmux-command "kill-window" "-t" target)
        (remhash target tmux-manager--marks))
      (tmux-manager-refresh)
      (message "Killed %d tmux window(s)" (length targets)))))

;;;###autoload
(defun tmux-manager ()
  "Open tmux manager buffer for sync/jump/rename/bulk-delete."
  (interactive)
  (unless (tmux-manager--tmux-available-p)
    (user-error "tmux is not running"))
  (let ((buf (get-buffer-create tmux-manager-buffer-name)))
    (with-current-buffer buf
      (tmux-manager-mode)
      (tmux-manager-refresh))
    (switch-to-buffer buf)))

;;;###autoload
(defun tmux-manager-switch-to-buffer ()
  "Switch to tmux manager buffer, creating it if needed."
  (interactive)
  (let ((buf (get-buffer tmux-manager-buffer-name)))
    (if buf
        (switch-to-buffer buf)
      (tmux-manager))))

(provide 'tmux-manager)

;;; tmux-manager.el ends here
