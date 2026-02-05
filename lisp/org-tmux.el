;; -*- lexical-binding: t; -*-
;;; org-tmux.el --- Org-mode integration for tmux window management

;; Author: Pandaye
;; Keywords: org, tmux, convenience

;;; Commentary:
;; This package provides org-mode link type for tmux windows/panes,
;; window synchronization, and pane merging functionality.
;;
;; Link formats:
;;   [[tmux:window-name]]           - Window by name in current session
;;   [[tmux:3]]                     - Window by index
;;   [[tmux:session:window]]        - Explicit session:window
;;   [[tmux:session:window.pane]]   - Session:window.pane
;;
;; Commands:
;;   org-tmux-goto-window           - Jump to any tmux window
;;   org-tmux-sync-windows          - Insert window list at point
;;   org-tmux-sync-windows-to-buffer - Create buffer with all windows
;;   org-tmux-join-panes-interactive - Merge two panes
;;   org-tmux-break-pane-interactive - Break pane into new window
;;   org-tmux-insert-link           - Insert tmux link at point

;;; Code:

(require 'org)
(require 'ol)

;;; Customization

(defgroup org-tmux nil
  "Org-mode integration for tmux."
  :group 'org-link
  :prefix "org-tmux-")

(defcustom org-tmux-default-session nil
  "Default tmux session to use when not specified.
If nil, use the currently attached session."
  :type '(choice (const :tag "Current attached session" nil)
                 (string :tag "Session name"))
  :group 'org-tmux)

(defcustom org-tmux-window-format "#{session_name}:#{window_index}:#{window_name}:#{window_active}"
  "Format string for listing tmux windows."
  :type 'string
  :group 'org-tmux)

(defcustom org-tmux-pane-format "#{session_name}:#{window_index}.#{pane_index}:#{pane_current_command}:#{pane_active}"
  "Format string for listing tmux panes."
  :type 'string
  :group 'org-tmux)

(defcustom org-tmux-sync-format 'headings
  "Format for window synchronization output.
- headings: Org headings grouped by session
- flat: Flat list with session prefix
- table: Org table format"
  :type '(choice (const :tag "Org headings by session" headings)
                 (const :tag "Flat list" flat)
                 (const :tag "Org table" table))
  :group 'org-tmux)

;;; Utility Functions

(defun org-tmux--tmux-available-p ()
  "Check if tmux is available and running."
  (and (executable-find "tmux")
       (= 0 (call-process "tmux" nil nil nil "list-sessions"))))

(defun org-tmux--run-tmux-command (&rest args)
  "Run tmux command with ARGS and return output as string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "tmux" nil t nil args)))
      (if (= exit-code 0)
          (string-trim (buffer-string))
        nil))))

(defun org-tmux--get-current-session ()
  "Get the name of the currently attached tmux session."
  (or org-tmux-default-session
      (org-tmux--run-tmux-command "display-message" "-p" "#{session_name}")))

(defun org-tmux--list-sessions ()
  "Return a list of tmux session names."
  (when (org-tmux--tmux-available-p)
    (let ((output (org-tmux--run-tmux-command "list-sessions" "-F" "#{session_name}")))
      (when output
        (split-string output "\n" t)))))

(defun org-tmux--list-windows (&optional session)
  "Return a list of windows for SESSION (or all sessions if nil).
Each element is a plist with :session, :index, :name, :active."
  (when (org-tmux--tmux-available-p)
    (let* ((args (if session
                     (list "list-windows" "-t" session "-F" org-tmux-window-format)
                   (list "list-windows" "-a" "-F" org-tmux-window-format)))
           (output (apply #'org-tmux--run-tmux-command args)))
      (when output
        (mapcar (lambda (line)
                  (let ((parts (split-string line ":")))
                    (list :session (nth 0 parts)
                          :index (string-to-number (nth 1 parts))
                          :name (nth 2 parts)
                          :active (string= (nth 3 parts) "1"))))
                (split-string output "\n" t))))))

(defun org-tmux--list-panes (&optional target)
  "Return a list of panes for TARGET (session:window or all if nil).
Each element is a plist with :session, :window, :pane, :command, :active."
  (when (org-tmux--tmux-available-p)
    (let* ((args (if target
                     (list "list-panes" "-t" target "-F" org-tmux-pane-format)
                   (list "list-panes" "-a" "-F" org-tmux-pane-format)))
           (output (apply #'org-tmux--run-tmux-command args)))
      (when output
        (mapcar (lambda (line)
                  (let* ((parts (split-string line ":"))
                         (win-pane (split-string (nth 1 parts) "\\.")))
                    (list :session (nth 0 parts)
                          :window (string-to-number (car win-pane))
                          :pane (string-to-number (or (cadr win-pane) "0"))
                          :command (nth 2 parts)
                          :active (string= (nth 3 parts) "1"))))
                (split-string output "\n" t))))))

(defun org-tmux--window-exists-p (target)
  "Check if TARGET window exists.
TARGET can be session:window or just window (uses current session)."
  (= 0 (call-process "tmux" nil nil nil "list-windows" "-t" target)))

(defun org-tmux--parse-link (link)
  "Parse LINK into (session . window) or (session . window.pane).
Returns a plist with :session, :window, :pane (pane may be nil)."
  (let* ((parts (split-string link ":"))
         session window pane)
    (cond
     ;; session:window or session:window.pane
     ((= (length parts) 2)
      (setq session (car parts))
      (let ((win-pane (split-string (cadr parts) "\\.")))
        (setq window (car win-pane))
        (setq pane (cadr win-pane))))
     ;; just window or window.pane (use current session)
     ((= (length parts) 1)
      (setq session (org-tmux--get-current-session))
      (let ((win-pane (split-string (car parts) "\\.")))
        (setq window (car win-pane))
        (setq pane (cadr win-pane)))))
    (list :session session :window window :pane pane)))

(defun org-tmux--build-target (parsed)
  "Build tmux target string from PARSED plist."
  (let ((session (plist-get parsed :session))
        (window (plist-get parsed :window))
        (pane (plist-get parsed :pane)))
    (concat session ":" window
            (when pane (concat "." pane)))))

;;; Link Type Definition

(defun org-tmux--follow (link &optional _prefix-arg)
  "Follow a tmux LINK by switching to that window/pane."
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((parsed (org-tmux--parse-link link))
         (target (org-tmux--build-target parsed))
         (session (plist-get parsed :session))
         (window (plist-get parsed :window))
         (pane (plist-get parsed :pane)))
    ;; First switch to the session if different
    (let ((current-session (org-tmux--get-current-session)))
      (unless (string= session current-session)
        (org-tmux--run-tmux-command "switch-client" "-t" session)))
    ;; Then select the window
    (org-tmux--run-tmux-command "select-window" "-t" (concat session ":" window))
    ;; If pane specified, select it
    (when pane
      (org-tmux--run-tmux-command "select-pane" "-t" target))
    (message "Switched to tmux %s" target)))

(defun org-tmux--export (link desc backend _info)
  "Export a tmux LINK with DESC for BACKEND."
  (let ((description (or desc link)))
    (pcase backend
      ('html (format "<code>tmux:%s</code>" description))
      ('latex (format "\\texttt{tmux:%s}" description))
      ('ascii (format "tmux:%s" description))
      (_ description))))

(defun org-tmux--complete (&optional _prefix-arg)
  "Complete a tmux link by selecting from available windows."
  (let* ((windows (org-tmux--list-windows))
         (candidates (mapcar (lambda (w)
                               (format "%s:%d %s"
                                       (plist-get w :session)
                                       (plist-get w :index)
                                       (plist-get w :name)))
                             windows))
         (choice (completing-read "tmux window: " candidates nil t))
         (parts (split-string choice " ")))
    ;; Return just session:index
    (car parts)))

;; Register the tmux link type
(org-link-set-parameters
 "tmux"
 :follow #'org-tmux--follow
 :export #'org-tmux--export
 :complete #'org-tmux--complete
 :face 'org-link)

;;; Interactive Commands

;;;###autoload
(defun org-tmux-goto-window ()
  "Interactively select and switch to a tmux window."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((windows (org-tmux--list-windows))
         (candidates (mapcar (lambda (w)
                               (cons (format "%s:%d %s%s"
                                             (plist-get w :session)
                                             (plist-get w :index)
                                             (plist-get w :name)
                                             (if (plist-get w :active) " *" ""))
                                     w))
                             windows))
         (choice (completing-read "Switch to window: " candidates nil t))
         (selected (cdr (assoc choice candidates))))
    (when selected
      (let ((target (format "%s:%d"
                            (plist-get selected :session)
                            (plist-get selected :index))))
        (org-tmux--follow target)))))

;;;###autoload
(defun org-tmux-insert-link ()
  "Insert a tmux link at point by selecting from available windows."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((windows (org-tmux--list-windows))
         (candidates (mapcar (lambda (w)
                               (cons (format "%s:%d %s"
                                             (plist-get w :session)
                                             (plist-get w :index)
                                             (plist-get w :name))
                                     w))
                             windows))
         (choice (completing-read "Select window: " candidates nil t))
         (selected (cdr (assoc choice candidates))))
    (when selected
      (let* ((target (format "%s:%d"
                             (plist-get selected :session)
                             (plist-get selected :index)))
             (name (plist-get selected :name))
             (desc (read-string (format "Description [%s]: " name) nil nil name)))
        (insert (format "[[tmux:%s][%s]]" target desc))))))

;;;###autoload
(defun org-tmux-sync-windows ()
  "Insert a list of all tmux windows at point."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let ((windows (org-tmux--list-windows))
        (current-session (org-tmux--get-current-session)))
    (pcase org-tmux-sync-format
      ('headings (org-tmux--sync-as-headings windows current-session))
      ('flat (org-tmux--sync-as-flat windows))
      ('table (org-tmux--sync-as-table windows)))))

(defun org-tmux--sync-as-headings (windows current-session)
  "Insert WINDOWS as org headings grouped by session.
CURRENT-SESSION is marked as attached."
  (let ((sessions (delete-dups (mapcar (lambda (w) (plist-get w :session)) windows))))
    (dolist (session sessions)
      (insert (format "** Session: %s%s\n"
                      session
                      (if (string= session current-session) " (attached)" "")))
      (dolist (win windows)
        (when (string= (plist-get win :session) session)
          (let ((target (format "%s:%d" session (plist-get win :index))))
            (insert (format "- [[tmux:%s][%d: %s]]%s\n"
                            target
                            (plist-get win :index)
                            (plist-get win :name)
                            (if (plist-get win :active) " *" "")))))))))

(defun org-tmux--sync-as-flat (windows)
  "Insert WINDOWS as a flat list."
  (dolist (win windows)
    (let ((target (format "%s:%d"
                          (plist-get win :session)
                          (plist-get win :index))))
      (insert (format "- [[tmux:%s][%s:%d %s]]%s\n"
                      target
                      (plist-get win :session)
                      (plist-get win :index)
                      (plist-get win :name)
                      (if (plist-get win :active) " *" ""))))))

(defun org-tmux--sync-as-table (windows)
  "Insert WINDOWS as an org table."
  (insert "| Session | Index | Name | Active |\n")
  (insert "|---------|-------|------|--------|\n")
  (dolist (win windows)
    (let ((target (format "%s:%d"
                          (plist-get win :session)
                          (plist-get win :index))))
      (insert (format "| %s | [[tmux:%s][%d]] | %s | %s |\n"
                      (plist-get win :session)
                      target
                      (plist-get win :index)
                      (plist-get win :name)
                      (if (plist-get win :active) "✓" ""))))))

;;;###autoload
(defun org-tmux-sync-windows-to-buffer ()
  "Create a new buffer with all tmux windows listed."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let ((buf (get-buffer-create "*tmux-windows*"))
        (windows (org-tmux--list-windows))
        (current-session (org-tmux--get-current-session)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: tmux Windows\n")
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert "* tmux Windows\n\n")
        ;; Insert windows directly here instead of calling org-tmux-sync-windows
        (pcase org-tmux-sync-format
          ('headings (org-tmux--sync-as-headings windows current-session))
          ('flat (org-tmux--sync-as-flat windows))
          ('table (org-tmux--sync-as-table windows))))
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (goto-char (point-min)))
    (switch-to-buffer buf)))

;;;###autoload
(defun org-tmux-join-panes-interactive ()
  "Interactively select two panes and join them."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((panes (org-tmux--list-panes))
         (candidates (mapcar (lambda (p)
                               (cons (format "%s:%d.%d (%s)%s"
                                             (plist-get p :session)
                                             (plist-get p :window)
                                             (plist-get p :pane)
                                             (plist-get p :command)
                                             (if (plist-get p :active) " *" ""))
                                     p))
                             panes))
         ;; Select source pane
         (src-choice (completing-read "Source pane (to move): " candidates nil t))
         (src-pane (cdr (assoc src-choice candidates)))
         (src-target (format "%s:%d.%d"
                             (plist-get src-pane :session)
                             (plist-get src-pane :window)
                             (plist-get src-pane :pane)))
         ;; Remove source from candidates for destination
         (dst-candidates (remove (assoc src-choice candidates) candidates))
         (dst-choice (completing-read "Destination pane (to join into): " dst-candidates nil t))
         (dst-pane (cdr (assoc dst-choice dst-candidates)))
         (dst-target (format "%s:%d.%d"
                             (plist-get dst-pane :session)
                             (plist-get dst-pane :window)
                             (plist-get dst-pane :pane)))
         ;; Select orientation
         (orientation (completing-read "Join orientation: "
                                       '("vertical (stacked)" "horizontal (side-by-side)")
                                       nil t))
         (h-flag (if (string-prefix-p "horizontal" orientation) "-h" "-v")))
    (org-tmux--run-tmux-command "join-pane" h-flag "-s" src-target "-t" dst-target)
    (message "Joined %s into %s" src-target dst-target)))

;;;###autoload
(defun org-tmux-break-pane-interactive ()
  "Interactively select a pane and break it into a new window."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((panes (org-tmux--list-panes))
         (candidates (mapcar (lambda (p)
                               (cons (format "%s:%d.%d (%s)%s"
                                             (plist-get p :session)
                                             (plist-get p :window)
                                             (plist-get p :pane)
                                             (plist-get p :command)
                                             (if (plist-get p :active) " *" ""))
                                     p))
                             panes))
         (choice (completing-read "Break pane into window: " candidates nil t))
         (pane (cdr (assoc choice candidates)))
         (target (format "%s:%d.%d"
                         (plist-get pane :session)
                         (plist-get pane :window)
                         (plist-get pane :pane)))
         (name (read-string "New window name (empty for default): ")))
    (if (string-empty-p name)
        (org-tmux--run-tmux-command "break-pane" "-s" target)
      (org-tmux--run-tmux-command "break-pane" "-s" target "-n" name))
    (message "Broke pane %s into new window" target)))

;;;###autoload
(defun org-tmux-rename-window ()
  "Interactively rename a tmux window."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((windows (org-tmux--list-windows))
         (candidates (mapcar (lambda (w)
                               (cons (format "%s:%d %s"
                                             (plist-get w :session)
                                             (plist-get w :index)
                                             (plist-get w :name))
                                     w))
                             windows))
         (choice (completing-read "Rename window: " candidates nil t))
         (win (cdr (assoc choice candidates)))
         (target (format "%s:%d"
                         (plist-get win :session)
                         (plist-get win :index)))
         (new-name (read-string "New name: " (plist-get win :name))))
    (org-tmux--run-tmux-command "rename-window" "-t" target new-name)
    (message "Renamed window %s to %s" target new-name)))

;;;###autoload
(defun org-tmux-kill-window ()
  "Interactively kill a tmux window."
  (interactive)
  (unless (org-tmux--tmux-available-p)
    (user-error "tmux is not running"))
  (let* ((windows (org-tmux--list-windows))
         (candidates (mapcar (lambda (w)
                               (cons (format "%s:%d %s"
                                             (plist-get w :session)
                                             (plist-get w :index)
                                             (plist-get w :name))
                                     w))
                             windows))
         (choice (completing-read "Kill window: " candidates nil t))
         (win (cdr (assoc choice candidates)))
         (target (format "%s:%d"
                         (plist-get win :session)
                         (plist-get win :index))))
    (when (yes-or-no-p (format "Really kill window %s? " target))
      (org-tmux--run-tmux-command "kill-window" "-t" target)
      (message "Killed window %s" target))))

;;;###autoload
(defun org-tmux-switch-to-buffer ()
  "Switch to *tmux-windows* buffer, create if not exists."
  (interactive)
  (let ((buf (get-buffer "*tmux-windows*")))
    (if buf
        (switch-to-buffer buf)
      (org-tmux-sync-windows-to-buffer))))

;;; Keybindings (C-c d 前缀, 全局可用):
;;
;;   C-c d d / SPC d d  - 快速跳转到 *tmux-windows* buffer
;;   C-c d g / SPC d g  - 跳转到任意 tmux 窗口
;;   C-c d l / SPC d l  - 插入 tmux 链接
;;   C-c d s / SPC d s  - 在当前位置插入窗口列表
;;   C-c d S / SPC d S  - 新建 buffer 显示窗口列表
;;   C-c d j / SPC d j  - 合并 panes
;;   C-c d b / SPC d b  - 拆分 pane 为窗口
;;   C-c d r / SPC d r  - 重命名窗口
;;   C-c d k / SPC d k  - 关闭窗口

;; 全局快捷键 (C-c d 前缀)
(global-set-key (kbd "C-c d d") 'org-tmux-switch-to-buffer)
(global-set-key (kbd "C-c d g") 'org-tmux-goto-window)
(global-set-key (kbd "C-c d l") 'org-tmux-insert-link)
(global-set-key (kbd "C-c d s") 'org-tmux-sync-windows)
(global-set-key (kbd "C-c d S") 'org-tmux-sync-windows-to-buffer)
(global-set-key (kbd "C-c d j") 'org-tmux-join-panes-interactive)
(global-set-key (kbd "C-c d b") 'org-tmux-break-pane-interactive)
(global-set-key (kbd "C-c d r") 'org-tmux-rename-window)
(global-set-key (kbd "C-c d k") 'org-tmux-kill-window)

(provide 'org-tmux)

;;; org-tmux.el ends here
