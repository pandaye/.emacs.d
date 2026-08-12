;; -*- lexical-binding: t; -*-
(require 'org)
(require 'subr-x)

(defun org-ssh-get-file-properties ()
  "获取当前文件的全局属性。"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((props '()))
        (while (re-search-forward "^\\s-*#\\+PROPERTY:\\s-+\\([^[:space:]]+\\)\\s-+\\(.*\\)$" nil t)
          (let ((key (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (push (cons key value) props)))
        props))))

(defun org-ssh-get-ssh-config (host)
  "获取当前文件的 SSH 配置，合并文件级和条目级属性。"
  (let* ((file-props (org-ssh-get-file-properties))
         ;; 优先级：条目属性 > 文件属性 > 默认值
         (user (or (org-entry-get nil "SSH_USER")
                   (cdr (assoc "SSH_USER" file-props))
                   "root"))
         (port (or (org-entry-get nil "SSH_PORT")
                   (cdr (assoc "SSH_PORT" file-props))
                   "22"))
         (password (cdr (assoc "SSH_PASSWORD" file-props)))
         (key-file (cdr (assoc "SSH_KEY_FILE" file-props)))
         (clean-known-hosts (or (org-entry-get nil "SSH_CLEAN_KNOWN_HOSTS")
                                (cdr (assoc "SSH_CLEAN_KNOWN_HOSTS" file-props))))
         (proxy-jump (or (org-entry-get nil "SSH_PROXY_JUMP")
                         (cdr (assoc "SSH_PROXY_JUMP" file-props))))
         (proxy-command (or (org-entry-get nil "SSH_PROXY_COMMAND")
                            (cdr (assoc "SSH_PROXY_COMMAND" file-props)))))
    (message "[org-ssh] %s:%s" host port)
    (list :host host
          :user user  
          :port port
          :password password
          :key-file key-file
          :clean-known-hosts (and clean-known-hosts 
                                  (not (string-empty-p clean-known-hosts))
                                  (not (string= clean-known-hosts "no")))
          :proxy-jump (and proxy-jump (not (string-empty-p proxy-jump)) proxy-jump)
          :proxy-command (and proxy-command (not (string-empty-p proxy-command)) proxy-command))))

(defun org-ssh-build-ssh-command-from-config (config)
  "根据配置构建 SSH 命令。"
  (let* ((host (plist-get config :host))
         (user (plist-get config :user))
         (port (plist-get config :port))
         (password (plist-get config :password))
         (key-file (plist-get config :key-file))
         (clean-known-hosts (plist-get config :clean-known-hosts))
         (proxy-jump (plist-get config :proxy-jump))
         (proxy-command (plist-get config :proxy-command))
         (ssh-base (format "ssh %s@%s -p %s" user host port))
         (ssh-with-key (if key-file 
                           (format "%s -i %s" ssh-base key-file)
                         ssh-base))
         (ssh-with-proxy (cond
                          ;; 优先使用 ProxyJump（更简洁、更现代）
                          (proxy-jump
                           (format "%s -J %s" ssh-with-key proxy-jump))
                          ;; 其次使用 ProxyCommand
                          (proxy-command
                           (format "%s -o \"ProxyCommand=%s\"" ssh-with-key proxy-command))
                          ;; 无代理
                          (t ssh-with-key)))
         (ssh-with-options (if clean-known-hosts
                               (format "%s -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no" ssh-with-proxy)
                             ssh-with-proxy))
         (final-cmd (if password
                        (format "sshpass -p '%s' %s" password ssh-with-options)
                      ssh-with-options)))
    ;; 调试输出
    (message "[org-ssh DEBUG] Final command: %s" final-cmd)
    final-cmd))

(defun org-ssh-connect-via-tmux (host)
  "使用文件级配置连接到指定主机。"
  (let* ((config (org-ssh-get-ssh-config host))
         (ssh-cmd (org-ssh-build-ssh-command-from-config config))
         (window-name (format "%s" host))
         (tmux-cmd (format "tmux new-window -n '%s' '%s'" window-name ssh-cmd)))
    (message "[org-ssh] Access SSH: %s" host)
    (start-process-shell-command "tmux-ssh" nil tmux-cmd)))

(defun org-ssh-debug-command (host)
  "调试：显示将要执行的 SSH 命令，但不实际连接。"
  (interactive "sHost: ")
  (let* ((config (org-ssh-get-ssh-config host))
         (ssh-cmd (org-ssh-build-ssh-command-from-config config))
         (window-name (format "%s" host))
         (tmux-cmd (format "tmux new-window -n '%s' '%s'" window-name ssh-cmd)))
    (with-output-to-temp-buffer "*SSH Debug*"
      (princ "=== SSH Configuration ===\n")
      (princ (format "Host: %s\n" (plist-get config :host)))
      (princ (format "User: %s\n" (plist-get config :user)))
      (princ (format "Port: %s\n" (plist-get config :port)))
      (princ (format "Password: %s\n" (if (plist-get config :password) "***" "None")))
      (princ (format "Key File: %s\n" (or (plist-get config :key-file) "None")))
      (princ (format "Clean Known Hosts: %s\n" (plist-get config :clean-known-hosts)))
      (princ (format "Proxy Jump: %s\n" (or (plist-get config :proxy-jump) "None")))
      (princ (format "Proxy Command: %s\n" (or (plist-get config :proxy-command) "None")))
      (princ "\n=== Generated SSH Command ===\n")
      (princ (format "%s\n" ssh-cmd))
      (princ "\n=== Full Tmux Command ===\n")
      (princ (format "%s\n" tmux-cmd))
      (princ "\n=== Test Command (Run in terminal) ===\n")
      (princ (format "%s\n" ssh-cmd)))))

;; 注册自定义链接类型
(org-link-set-parameters
 "ssh"
 :follow (lambda (host)
           (org-ssh-connect-via-tmux host))
 :export (lambda (host desc backend)
           (format "SSH: %s" (or desc host))))

;; 快速创建新的服务器组文件

;; 配置 SSH 服务器组文件目录变量
(defvaralias 'my/ssh-configs-dir 'org-ssh-configs-dir)

(defcustom org-ssh-configs-dir (expand-file-name "~/work/hosts/")
  "存放 SSH 服务器组 org 文件的目录。"
  :type 'string
  :group 'convenience)

;; 优化后的模板创建函数
(defun org-ssh-create-template (filename group-name default-user default-port)
  "创建新的 SSH 服务器组文件模板。"
  (interactive 
   (list (read-file-name "文件名: " org-ssh-configs-dir nil nil ".org")
         (read-string "服务器组名称: ")
         (read-string "默认用户名: " user-login-name)
         (read-string "默认端口: " "22")))
  (with-temp-buffer
    (insert (format "#+TITLE: %s\n" group-name))
    (insert (format "#+PROPERTY: SSH_USER %s\n" default-user))
    (insert (format "#+PROPERTY: SSH_PORT %s\n" default-port))
    (insert "#+PROPERTY: SSH_PASSWORD \n")
    (insert "#+PROPERTY: SSH_KEY_FILE \n")
    (insert "#+PROPERTY: SSH_CLEAN_KNOWN_HOSTS yes\n")
    (insert "#+PROPERTY: SSH_PROXY_JUMP \n")
    (insert "#+PROPERTY: SSH_PROXY_COMMAND \n\n")
    (insert (format "* %s\n\n" group-name))
    (insert "配置说明：\n")
    (insert "- SSH_CLEAN_KNOWN_HOSTS: yes/no，是否忽略 known_hosts 检查（适用于虚拟机等场景）\n")
    (insert "- SSH_PROXY_JUMP: 跳板机配置，例如 user@jumphost:port\n")
    (insert "- SSH_PROXY_COMMAND: 自定义代理命令，例如 'nc -X 5 -x proxy:1080 %h %p'（SOCKS5）\n")
    (insert "- 条目级属性会覆盖文件级属性\n\n")
    (insert "- [[ssh:][Server-01]]\n")
    (write-file filename))
  (find-file filename))

;; 新增：在 org-ssh-configs-dir 目录下选择并打开服务器组文件
(defun org-ssh-open-group-file ()
  "在 org-ssh-configs-dir 目录下选择并打开一个 SSH 服务器组 org 文件。"
  (interactive)
  (let* ((file (read-file-name "选择服务器组文件: " org-ssh-configs-dir nil t nil
                               (lambda (f) (string-match-p "\\.org$" f)))))
    (when (and file (file-exists-p file))
      (find-file file))))

;; 批量连接当前文件的所有服务器
(defun org-ssh-connect-all-servers-in-file ()
  "连接当前文件中的所有 SSH 服务器。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((servers '()))
      ;; 收集所有 ssh 链接
      (while (re-search-forward "\\[\\[ssh:\\([^]]+\\)\\]" nil t)
        (push (match-string 1) servers))
      ;; 确认并连接
      (when (and servers 
                 (y-or-n-p (format "连接 %d 个服务器？" (length servers))))
        (dolist (server (reverse servers))
          (org-ssh-connect-via-tmux server)
          (sit-for 0.3)))))) ; 间隔避免过快

;; 快速添加新服务器到当前组
(defun org-ssh-add-server-to-current-group ()
  "在当前位置添加新的服务器链接。"
  (interactive)
  (let* ((host (read-string "服务器地址: "))
         (name (read-string "显示名称: "))
         (display-name (if (string-empty-p name) host name)))
    (insert (format "[[ssh:%s][%s]]" host display-name))))

(defun org-ssh-insert-link-and-open (host &optional name)
  "Insert an Org SSH link for HOST with NAME and open it in tmux."
  (unless (and host (not (string-empty-p host)))
    (user-error "Host is required"))
  (let ((display-name (if (and name (not (string-empty-p name))) name host)))
	(insert "| ")
    (insert (org-link-make-string (concat "ssh:" host) display-name))
	(insert "\n  ")
    (org-ssh-connect-via-tmux host)))

(defun org-ssh-create-link-and-open (host name)
  "Create a new Org SSH link at point and open HOST in tmux."
  (interactive
   (let* ((host (read-string "服务器地址: "))
          (name (read-string "显示名称: " host)))
     (list host name)))
  (org-ssh-insert-link-and-open host name))

;; 显示当前文件的 SSH 配置摘要
(defun org-ssh-show-ssh-config-summary ()
  "显示当前文件的 SSH 配置摘要。"
  (interactive)
  (let* ((file-props (org-ssh-get-file-properties))
         (user (cdr (assoc "SSH_USER" file-props)))
         (port (cdr (assoc "SSH_PORT" file-props)))
         (has-password (not (string-empty-p (or (cdr (assoc "SSH_PASSWORD" file-props)) ""))))
         (key-file (cdr (assoc "SSH_KEY_FILE" file-props)))
         (clean-known-hosts (cdr (assoc "SSH_CLEAN_KNOWN_HOSTS" file-props)))
         (proxy-jump (cdr (assoc "SSH_PROXY_JUMP" file-props)))
         (proxy-command (cdr (assoc "SSH_PROXY_COMMAND" file-props))))
    (message "SSH配置 - 用户:%s 端口:%s 密码:%s 密钥:%s 清理known_hosts:%s 代理跳转:%s 代理命令:%s" 
             (or user "默认") 
             (or port "22")
             (if has-password "是" "否")
             (or key-file "无")
             (if (and clean-known-hosts 
                      (not (string-empty-p clean-known-hosts))
                      (not (string= clean-known-hosts "no"))) "是" "否")
             (if (and proxy-jump (not (string-empty-p proxy-jump))) proxy-jump "无")
             (if (and proxy-command (not (string-empty-p proxy-command))) "已配置" "无"))))


(dolist (names '((my/tmux-ssh-connect-simple . org-ssh-connect-via-tmux)
                 (my/debug-ssh-command . org-ssh-debug-command)
                 (my/create-ssh-template . org-ssh-create-template)
                 (my/open-ssh-group-file . org-ssh-open-group-file)
                 (my/connect-all-servers-in-file . org-ssh-connect-all-servers-in-file)
                 (my/add-server-to-current-group . org-ssh-add-server-to-current-group)
                 (my/org-ssh-insert-link-and-open . org-ssh-insert-link-and-open)
                 (my/org-ssh-create-link-and-open . org-ssh-create-link-and-open)
                 (my/show-ssh-config-summary . org-ssh-show-ssh-config-summary)))
  (defalias (car names) (cdr names)))

(provide 'org-ssh)
