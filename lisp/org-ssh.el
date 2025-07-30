(require 'org)

(defun my/get-file-properties ()
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

(defun my/get-ssh-config (host)
  "获取当前文件的 SSH 配置，合并文件级和条目级属性。"
  (let* ((file-props (my/get-file-properties))
         (entry-props (org-entry-properties))
         ;; 优先级：条目属性 > 文件属性 > 默认值
         (user (or (cdr (assoc "SSH_USER" entry-props))
                   (cdr (assoc "SSH_USER" file-props))
                   "root"))
         (port (or (cdr (assoc "SSH_PORT" entry-props))
                   (cdr (assoc "SSH_PORT" file-props))
                   "22"))
         (password (or (cdr (assoc "SSH_PASSWORD" entry-props))
                       (cdr (assoc "SSH_PASSWORD" file-props))))
         (key-file (or (cdr (assoc "SSH_KEY_FILE" entry-props))
                       (cdr (assoc "SSH_KEY_FILE" file-props))))
         (clean-known-hosts (or (cdr (assoc "SSH_CLEAN_KNOWN_HOSTS" entry-props))
                                (cdr (assoc "SSH_CLEAN_KNOWN_HOSTS" file-props)))))
    (list :host host
          :user user  
          :port port
          :password password
          :key-file key-file
          :clean-known-hosts (and clean-known-hosts 
                                  (not (string-empty-p clean-known-hosts))
                                  (not (string= clean-known-hosts "no"))))))

(defun my/build-ssh-command-from-config (config)
  "根据配置构建 SSH 命令。"
  (let* ((host (plist-get config :host))
         (user (plist-get config :user))
         (port (plist-get config :port))
         (password (plist-get config :password))
         (key-file (plist-get config :key-file))
         (clean-known-hosts (plist-get config :clean-known-hosts))
         (ssh-base (format "ssh %s@%s -p %s" user host port))
         (ssh-with-key (if key-file 
                           (format "%s -i %s" ssh-base key-file)
                         ssh-base))
         (ssh-with-options (if clean-known-hosts
                               (format "%s -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no" ssh-with-key)
                             ssh-with-key)))
    ;; 如果有密码使用 sshpass，否则假设使用密钥
    (if password
        (format "sshpass -p '%s' %s" password ssh-with-options)
      ssh-with-options)))

(defun my/tmux-ssh-connect-simple (host)
  "使用文件级配置连接到指定主机。"
  (let* ((config (my/get-ssh-config host))
         (ssh-cmd (my/build-ssh-command-from-config config))
         (window-name (format "%s" host))
         (tmux-cmd (format "tmux new-window -n '%s' '%s'" window-name ssh-cmd)))
    (message "Access SSH: %s" host)
    (start-process-shell-command "tmux-ssh" nil tmux-cmd)))

;; 注册自定义链接类型
(org-link-set-parameters
 "ssh"
 :follow (lambda (host)
           (my/tmux-ssh-connect-simple host))
 :export (lambda (host desc backend)
           (format "SSH: %s" (or desc host))))

;; 快速创建新的服务器组文件

;; 配置 SSH 服务器组文件目录变量
(defvar my/ssh-configs-dir (expand-file-name "~/Workspace/ssh-configs/")
  "存放 SSH 服务器组 org 文件的目录。")

;; 优化后的模板创建函数
(defun my/create-ssh-template (filename group-name default-user default-port)
  "创建新的 SSH 服务器组文件模板。"
  (interactive 
   (list (read-file-name "文件名: " my/ssh-configs-dir nil nil ".org")
         (read-string "服务器组名称: ")
         (read-string "默认用户名: " user-login-name)
         (read-string "默认端口: " "22")))
  (with-temp-buffer
    (insert (format "#+TITLE: %s\n" group-name))
    (insert (format "#+PROPERTY: SSH_USER %s\n" default-user))
    (insert (format "#+PROPERTY: SSH_PORT %s\n" default-port))
    (insert "#+PROPERTY: SSH_PASSWORD \n")
    (insert "#+PROPERTY: SSH_KEY_FILE \n")
    (insert "#+PROPERTY: SSH_CLEAN_KNOWN_HOSTS yes\n\n")
    (insert (format "* %s\n\n" group-name))
    (insert "配置说明：\n")
    (insert "- SSH_CLEAN_KNOWN_HOSTS: yes/no，是否忽略 known_hosts 检查（适用于虚拟机等场景）\n")
    (insert "- 条目级属性会覆盖文件级属性\n\n")
    (insert "- [[ssh:][Server-01]]\n")
    (write-file filename))
  (find-file filename))

;; 新增：在 my/ssh-configs-dir 目录下选择并打开服务器组文件
(defun my/open-ssh-group-file ()
  "在 my/ssh-configs-dir 目录下选择并打开一个 SSH 服务器组 org 文件。"
  (interactive)
  (let* ((file (read-file-name "选择服务器组文件: " my/ssh-configs-dir nil t nil
                               (lambda (f) (string-match-p "\\.org$" f)))))
    (when (and file (file-exists-p file))
      (find-file file))))

;; 批量连接当前文件的所有服务器
(defun my/connect-all-servers-in-file ()
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
          (my/tmux-ssh-connect-simple server)
          (sit-for 0.3)))))) ; 间隔避免过快

;; 快速添加新服务器到当前组
(defun my/add-server-to-current-group ()
  "在当前位置添加新的服务器链接。"
  (interactive)
  (let* ((host (read-string "服务器地址: "))
         (name (read-string "显示名称: "))
         (display-name (if (string-empty-p name) host name)))
    (insert (format "[[ssh:%s][%s]]" host display-name))))

;; 显示当前文件的 SSH 配置摘要
(defun my/show-ssh-config-summary ()
  "显示当前文件的 SSH 配置摘要。"
  (interactive)
  (let* ((file-props (my/get-file-properties))
         (user (cdr (assoc "SSH_USER" file-props)))
         (port (cdr (assoc "SSH_PORT" file-props)))
         (has-password (not (string-empty-p (or (cdr (assoc "SSH_PASSWORD" file-props)) ""))))
         (key-file (cdr (assoc "SSH_KEY_FILE" file-props)))
         (clean-known-hosts (cdr (assoc "SSH_CLEAN_KNOWN_HOSTS" file-props))))
    (message "SSH配置 - 用户:%s 端口:%s 密码:%s 密钥:%s 清理known_hosts:%s" 
             (or user "默认") 
             (or port "22")
             (if has-password "是" "否")
             (or key-file "无")
             (if (and clean-known-hosts 
                      (not (string-empty-p clean-known-hosts))
                      (not (string= clean-known-hosts "no"))) "是" "否"))))


;; 在 org-mode 中添加便捷键绑定

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c s c") 'my/connect-all-servers-in-file)
     (define-key org-mode-map (kbd "C-c s a") 'my/add-server-to-current-group)
     (define-key org-mode-map (kbd "C-c s s") 'my/show-ssh-config-summary)
     (define-key org-mode-map (kbd "C-c s n") 'my/create-ssh-template)
     (define-key org-mode-map (kbd "C-c s o") 'my/open-ssh-group-file)))

(provide 'org-ssh)

