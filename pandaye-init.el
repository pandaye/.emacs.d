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

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :config
  (setq projectile-project-search-path '("~/Project/"))
  (setq projectile-completion-system 'auto)
  (setq projectile-mode-line
        '(:eval (format " Proj[%s]" (projectile-project-name)))))

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
                                (counsel-find-file . ivy--regex-fuzzy)      ; 文件查找使用增强匹配
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
   ("C-c b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :init
  (counsel-mode 1)
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f f" . counsel-file-jump)))    ;; 递归查找文件（支持深度搜索）
   ;; ("C-c j"   . counsel-git-grep)
   ;; ("C-c k"   . counsel-ag)
   ;; ("C-x l"   . counsel-locate)
   ;; ("C-c g"   . counsel-git)
   ;; ("C-c G"   . counsel-git-grep)))  ;; 在 Git 仓库中查找文件

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
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode))

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  ;; Highlight changes on editing.
  (diff-hl-flydiff-mode)
  ;; Makes fringe and margin react to mouse clicks to show the curresponding hunk.
  (diff-hl-show-hunk-mouse-mode)
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#e9cd43"))))
  (diff-hl-insert ((t (:background "#03e94f"))))
  (diff-hl-delete ((t (:background "#f5597e")))))

;; macOS 终端下的剪贴板配置
(defun macos-terminal-clipboard-setup ()
  "Setup clipboard integration for terminal Emacs on macOS."
  (when (and (eq system-type 'darwin)
	     (not (display-graphic-p)))

    ;; 设置剪贴板复制函数
    (setq interprogram-cut-function
	  (lambda (text &optional push)
	    "Copy TEXT to macOS clipboard using pbcopy."
	    (let ((process-connection-type nil))
	      (let ((proc (start-process "pbcopy" nil "pbcopy")))
		(process-send-string proc text)
		(process-send-eof proc)))))

    ;; 设置剪贴板粘贴函数
    (setq interprogram-paste-function
	  (lambda ()
	    "Paste from macOS clipboard using pbpaste."
	    (shell-command-to-string "pbpaste")))

    ;; 启用剪贴板交互
    (setq select-enable-clipboard t
	  save-interprogram-paste-before-kill t)))

(macos-terminal-clipboard-setup)

(require 'org-ssh)
(require 'my-org-writing)

(use-package company
  :ensure t
  :init
  (global-company-mode 1)
  :config
  ;; 可选：补全菜单延迟、最小输入字符数等
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
	company-backends '((company-capf company-files))))

(use-package beancount
  :ensure t
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.bean\\'". beancount-mode))

(use-package lsp-mode
  :ensure t
  :hook (beancount-mode . lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "beancount-language-server")
    :major-modes '(beancount-mode)
    :server-id 'beancount-language-server
    :priority 10
    :initialization-options
    (lambda () (list :journal_file (concat (projectile-project-root) "main.bean")
                     :formatting (list :prefix_width 30
				       :currency_column 60
				       :number_currency_spacing 1
				       :account_amount_spacing 2)))))
  :commands (lsp lsp-deferred))

(unless (featurep 'org-tempo)
  (require 'org-tempo))

;; 快捷键设置，和 vscode 一致
(global-set-key (kbd "C-c f s") 'save-buffer)
(global-set-key (kbd "C-c w o") 'ace-window)
(global-set-key (kbd "C-c w 1") 'delete-other-windows)
(global-set-key (kbd "C-c w 2") 'split-window-below)
(global-set-key (kbd "C-c w 3") 'split-window-right)
(global-set-key (kbd "C-c w q") 'delete-window)
