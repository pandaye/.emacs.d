(setq org-fontify-quote-and-verse-blocks t)
(with-eval-after-load 'org
  (set-face-attribute 'org-quote nil
                      :foreground "gray70" ; 一个比纯黑稍亮的深灰色
                      :extend t))

;; 低可视度 block 标题
(set-face-attribute 'org-block-begin-line nil :foreground "gray35")
(set-face-attribute 'org-block-end-line nil :foreground "gray35")

(use-package rime
  :ensure t
  :init
  (setq rime-librime-root (expand-file-name "~/.emacs.d/librime"))
  :config
  (setq rime-disable-predicates
	  '(meow-not-insert-p
	    rime-predicate-after-alphabet-char-p
            rime-predicate-prog-in-code-p))
  :custom
  (default-input-method "rime"))

(global-set-key (kbd "C-c i") 'toggle-input-method)

;; --------
;; GTD 配置
;; --------
(setq org-todo-keywords
      '((sequence "TODO(t!)" "PROCESSING(p!)" "BLOCK(b!)" "LATER(l!)" "|" "DONE(d!)" "CANCEL(c@/!)")))

;; 设置任务样式
(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#66cccc"    :weight bold))
        ("BLOCK" . (:foreground "red"    :weight bold))
        ("LATER" . (:foreground "yellow"    :weight bold))
        ("PROCESSING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green"  :weight bold))
        ("CANCEL" . (:foreground "black"  :weight bold))
        ))

(setq org-base-path (expand-file-name "~/.pandaye-journal"))
(defvar org-gtd-file
  (concat org-base-path "/project.org"))

(defun gtd ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))

;; 加入到日程列表里 - 设置整个目录，自动包含所有 .org 文件
(setq org-agenda-files (list org-base-path))
(setq org-agenda-skip-timestamp-if-done t)

;; 快捷键设置
;; 设置 Org Agenda 快捷键
(global-set-key (kbd "C-c o g") 'gtd)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l c") 'org-store-link)
(global-set-key (kbd "C-c l r") 'org-clock-report)

;; 其他未归档的配置
;;
(font-lock-add-keywords
 'org-mode
 '(("^ *- State \\(\"[A-Z]+\"\\).*\\[.*\\]"
    0 'font-lock-comment-face t)))

(defvar daily-diary-base-path (concat org-base-path "/daily")
  "Base path for daily diary files.")

(defun open-today-diary ()
  "Open today's diary file in the format /path/to/daily/YY/MM-DD.org"
  (interactive)
  (let* ((today (current-time))
	 (year (format-time-string "%y" today))
	 (month-day (format-time-string "%m-%d" today))
	 (diary-dir (expand-file-name year daily-diary-base-path))
	 (diary-file (expand-file-name (concat month-day ".org") diary-dir)))

    ;; Create directory if it doesn't exist
    (unless (file-exists-p diary-dir)
      (make-directory diary-dir t))
    ;; Open the diary file
    (find-file diary-file)
    ;; If it's a new file, add a basic header
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Diary - %s\n"
		      (format-time-string "%Y-%m-%d %A" today)))
      (insert (format "#+DATE: %s\n\n"
		      (format-time-string "%Y-%m-%d" today)))
      (insert "* Today's Notes\n\n")
      (save-buffer))))

(defun open-diary-by-date (date-string)
  "Open diary file for a specific date.
DATE-STRING should be in format YYYY-MM-DD or MM-DD (current year assumed)."
  (interactive "sEnter date (YYYY-MM-DD or MM-DD): ")
  (let* ((parsed-date (if (string-match "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" date-string)
			  ;; MM-DD format, use current year
			  (concat (format-time-string "%Y-") date-string)
			;; Assume YYYY-MM-DD format
			date-string))
	 (date-time (date-to-time (concat parsed-date " 00:00:00")))
	 (year (format-time-string "%y" date-time))
	 (month-day (format-time-string "%m-%d" date-time))
	 (diary-dir (expand-file-name year daily-diary-base-path))
	 (diary-file (expand-file-name (concat month-day ".org") diary-dir)))
    ;; Create directory if it doesn't exist
    (unless (file-exists-p diary-dir)
      (make-directory diary-dir t))
    ;; Open the diary file
    (find-file diary-file)
    ;; If it's a new file, add a basic header
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Diary - %s\n"
		      (format-time-string "%Y-%m-%d %A" date-time)))
      (insert (format "#+DATE: %s\n\n"
		      (format-time-string "%Y-%m-%d" date-time)))
      (insert "* Today's Notes\n\n")
      (save-buffer))))

;; TODO: 可以像 agenda 那样直接点进到具体的文件中
(defun list-diary-files ()
  "List all diary files in a buffer."
  (interactive)
  (let* ((diary-buffer "*Diary Files*")
	 (year-dirs (directory-files daily-diary-base-path t "^[0-9]\\{2\\}$")))

    (with-output-to-temp-buffer diary-buffer
      (princ "Daily Diary Files:\n")
      (princ "==================\n\n")

      (dolist (year-dir year-dirs)
	(let* ((year (file-name-nondirectory year-dir))
	       (diary-files (directory-files year-dir t "\\.org$")))
	  (when diary-files
	    (princ (format "20%s:\n" year))
	    (dolist (file diary-files)
	      (let ((filename (file-name-sans-extension
			       (file-name-nondirectory file))))
		(princ (format "  %s (20%s-%s)\n" filename year filename))))
	    (princ "\n"))))

      (princ "\nCommands:\n")
      (princ "  M-x open-today-diary    - Open today's diary\n")
      (princ "  M-x open-diary-by-date  - Open diary by date\n")
      (princ "  M-x list-diary-files    - Show this list\n"))))

;; Optional: Add keybindings
(global-set-key (kbd "C-c o t") 'open-today-diary)
(global-set-key (kbd "C-c o d") 'open-diary-by-date)
(global-set-key (kbd "C-c o l") 'list-diary-files)


;; -----------------
;; org-roam 双向链接
;; -----------------
(use-package org-roam
  :ensure t
  ;; 自定义变量设置
  :custom
  ;; 设置你的 org-roam 笔记存放的根目录
  ;; 推荐使用 "~/org/roam" 或 "~/Documents/roam"
  ;; 请根据你的喜好修改此路径！
  (org-roam-directory (file-truename "~/.pandaye-journal/roam"))

  ;; 自定义链接的显示方式，使其更简洁
  (org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      ;; :if-new 设置了新文件的头部内容
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)
     ("b" "book" plain (file "~/org/roam/templates/book.txt")
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
			 "#+title: ${title}\n#+roam_tags: book\n\n")
      :unnarrowed t)
     ("p" "project" plain "* GOALS\n\n%?\n\n* TASKS\n\n* NOTES\n"
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
			 "#+title: ${title}\n#+roam_tags: project\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "\n\n* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :empty-lines-after 0
      :unnarrowed t)))

  ;; 配置快捷键
  :bind (;; ("C-c n l" . org-roam-buffer-toggle) ; 切换/显示反向链接缓冲区
	 ("C-c n f" . org-roam-node-find)     ; 查找笔记
	 ("C-c n i" . org-roam-node-insert)   ; 插入一个指向笔记的链接
	 ("C-c n c" . org-roam-capture)       ; 创建一个新的笔记 (非常重要!)
	 ("C-c n o" . org-roam-buffer-toggle) ; 打开 roam-bufer
	 ;; 每日笔记相关的快捷键
	 ("C-c n g" . org-roam-dailies-goto-today)         ; 
	 ("C-c n y" . org-roam-dailies-goto-yesterday)     ; 昨天的笔记
	 ("C-c n d" . org-roam-dailies-capture-today))     ; 创建到今天的笔记
  ;; 初始化 org-roam
  :config
  ;; 这是 Org-roam v2 的标准启动方式
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  ;; 推荐在 org-roam-mode 启动后，顺便启动 UI 的本地服务器
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start nil))


;; ------------
;; 日程管理优化
;; ------------
(use-package org-super-agenda
  :ensure t
  :init
  (org-super-agenda-mode))
  ;; :config
  ;; (setq org-super-agenda-groups
  ;;       '((:auto-parent t))))  ;; 自动按父 headline 分组


(provide 'my-org-writing)
