(add-to-list 'exec-path "/opt/homebrew/bin/")
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(when (display-graphic-p)
  ;; GUI 专用配置
  (load "gui.el"))

;; Default Font
; (set-face-attribute 'default nil :font "Fira Mono 13")

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; 安装 diminish 以支持 :diminish 关键字
(use-package diminish
  :ensure t)

;; 备份文件配置 - 禁用 ~ 后缀文件，保留自动保存
(setq make-backup-files nil)       ; 禁用 file~ 备份文件


;; Ivy 配置 - 优化导航体验
(use-package ivy
  :ensure t
  :diminish
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t
        ivy-wrap t
        ivy-height 15
        ;; 智能模糊匹配 - 更精确的匹配策略
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)            ; M-x 使用模糊匹配
                                (counsel-find-file . ivy--regex-fuzzy)       ; 文件查找使用增强匹配
                                (counsel-file-jump . ivy--regex-fuzzy)      ; 文件跳转使用模糊匹配
                                (swiper . ivy--regex-plus)                  ; 搜索使用增强匹配
                                (ivy-switch-buffer . ivy--regex-plus)       ; 缓冲区切换使用增强匹配
                                (t . ivy--regex-plus))                      ; 其他情况使用增强匹配
        ;; 忽略大小写
        ivy-case-fold-search-default t
        ;; 初始输入为空
        ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :init
  (counsel-mode 1)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ;; 递归查找文件（支持深度搜索）
   ("C-c f" . counsel-file-jump)
   ;; 在 Git 仓库中查找文件
   ("C-c G" . counsel-git-grep)))

(use-package swiper
  :ensure t
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper-backward)))

(use-package ivy-rich
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1)
  ;; 为 counsel-find-file 提供更丰富的信息显示
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(setq org-todo-keywords
      '((sequence "TODO(p!)" "PROCESSING(t!)" "BLOCK(s!)" "|" "DONE(d!)" "CANCEL(a@/!)")))

;; 设置任务样式
(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#66cccc"    :weight bold))
        ("BLOCK" . (:foreground "red"    :weight bold))
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

;; 设置 Org Agenda 快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
;; 将 GTD 快捷键改为 C-c o g，避免与 counsel-git 冲突
(global-set-key (kbd "C-c o g") 'gtd)

;; 加入到日程列表里
(setq org-agenda-files (list org-gtd-file))

;; org SSH 配置
(require 'org-ssh)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

;; 安装字体支持（可选，主要用于 GUI）
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))


(use-package ace-window
  :ensure t
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename org-base-path))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-super-agenda
  :ensure t
  :init
  (org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:auto-parent t))))  ;; 自动按父 headline 分组
