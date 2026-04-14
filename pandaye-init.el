;; -*- lexical-binding: t; -*-
(add-to-list 'exec-path "/opt/homebrew/bin/")
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(setq scroll-step 1
      scroll-conservatively 10000
      scroll-margin 0
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

(setopt cjk-ambiguous-chars-are-wide nil)

(when (display-graphic-p)
  ;; GUI 专用配置
  (load "gui.el" :noerror))

;; ============================================================
;; 基础工具 - 需要尽早加载
;; ============================================================

(use-package try
  :ensure t
  :commands (try))

(use-package which-key
  :ensure t
  :defer 2
  :config (which-key-mode))

;; 安装 diminish 以支持 :diminish 关键字
(use-package diminish
  :ensure t)

;; 备份文件配置 - 禁用 ~ 后缀文件，保留自动保存
(setq make-backup-files nil)

;; ============================================================
;; 导航框架 - Ivy/Counsel/Swiper（核心导航，保留急切加载）
;; ============================================================

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
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-plus)
                                (counsel-find-file . ivy--regex-plus)
                                (counsel-file-jump . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (ivy-switch-buffer . ivy--regex-plus)
                                (t . ivy--regex-plus))
        ivy-case-fold-search-default t
        ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-c b b" . ivy-switch-buffer)
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
   ("C-c f g" . counsel-git)
   ("C-c f G" . counsel-git-grep)
   ("C-c f f" . counsel-file-jump)))

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
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; ============================================================
;; 项目与文件管理 - 延迟加载
;; ============================================================

(use-package projectile
  :ensure t
  :defer 3
  :init
  (setq projectile-project-search-path '("~/Project/")
        projectile-completion-system 'auto)
  :config
  (projectile-mode 1)
  :bind
  (("C-c f p" . projectile-find-file)))

(use-package neotree
  :ensure t
  :commands (neotree-show neotree-toggle neotree-find)
  :bind
  ("C-c t p" . neotree-show)
  ("C-c t t" . neotree-toggle)
  :config
  (setq neo-smart-open t
        neo-vc-integration '(face char)))

(use-package rg
  :ensure t
  :defer t)

(defalias 'list-buffers 'ibuffer)

;; ============================================================
;; 窗口与编辑增强
;; ============================================================

;; 窗口布局记忆
(winner-mode 1)

;; 安装字体支持（可选，主要用于 GUI）
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :commands (all-the-icons-install-fonts))

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package rainbow-delimiters
  :ensure t
  :hook (scheme-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (racket-mode . rainbow-delimiters-mode))

;; ============================================================
;; Git 与版本控制
;; ============================================================

(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch-popup)
  :bind
  (("C-c j s" . magit-status)
   ("C-c j p" . magit-dispatch-popup)))

(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-show-hunk-mouse-mode)
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#e9cd43"))))
  (diff-hl-insert ((t (:background "#03e94f"))))
  (diff-hl-delete ((t (:background "#f5597e")))))

;; ============================================================
;; Org 与写作系统 - 延迟加载
;; ============================================================

(condition-case err
    (progn
      (require 'org-ssh)
      (require 'tmux-manager))
  (error (message "可选模块加载失败: %s" (error-message-string err))))

;; Org 系统 - 拆分为独立模块
(require 'my-org-writing)    ;; Org 外观美化
(require 'my-gtd)            ;; GTD 任务管理
(require 'my-rime)           ;; Rime 中文输入法
(require 'my-diary)          ;; 日记系统
(require 'my-org-roam)       ;; Org-roam 双向链接

;; ============================================================
;; 编程语言支持 - 按需加载
;; ============================================================

;; 括号编辑增强
(use-package paredit
  :ensure t
  :hook (racket-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (clojure-mode . paredit-mode)
  (eval-expression-minibuffer-setup . paredit-mode)
  (ielm-mode . paredit-mode))

(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode)
         ("\\.scrbl\\'" . racket-mode))
  :config
  (setq racket-run-in-background t)
  :hook
  (racket-mode . racket-xp-mode))

(use-package beancount
  :ensure t
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.bean\\'" . beancount-mode)
  :config
  (define-key beancount-mode-map (kbd "TAB") nil))

(use-package go-mode
  :ensure t
  :mode
  ("\\.go\\'" . go-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . enable-paredit-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package cmake-mode
  :ensure t
  :mode ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode))

(unless (featurep 'org-tempo)
  (require 'org-tempo))

;; ============================================================
;; Snippets - 延迟到编程模式
;; ============================================================

(use-package yasnippet
  :ensure t
  :defer 2
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; ============================================================
;; 日志与监控 - 按需加载
;; ============================================================

(use-package logview
  :ensure t
  :commands (logview-mode))

;; ============================================================
;; Common Lisp 开发环境 - 按需加载
;; ============================================================

(use-package slime
  :ensure t
  :commands (slime)
  :init
  (setq inferior-lisp-program "ros run")
  :mode
  ("\\.ros\\'" . lisp-mode)
  :config
  (slime-setup '(slime-fancy slime-tramp slime-asdf slime-xref-browser))
  (add-hook 'lisp-mode-hook (lambda () (subword-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda () (subword-mode 1))))

;; ============================================================
;; Markdown - 按需加载
;; ============================================================

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package ox-gfm
  :ensure ox-gfm
  :after markdown-mode)

;; ============================================================
;; LSP - 延迟到编程模式（最大性能提升）
;; ============================================================

;; LSP - 编程模式首次激活时加载（避免启动时加载重型 lsp-bridge）
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (featurep 'my-lsp)
              (require 'my-lsp nil t))))

;; ============================================================
;; AI 工具 - 延迟加载
;; ============================================================

(condition-case err
    (progn
      (require 'my-gptel)
      (require 'my-agent-shell))
  (error (message "AI 工具模块加载失败: %s" (error-message-string err))))

;; org-opencode: modular Org frontend for opencode AI agent
(add-to-list 'load-path (expand-file-name "lisp/org-opencode" user-emacs-directory))
(autoload 'org-opencode-mode "org-opencode" "Minor mode for opencode in Org buffers." t)

;; ============================================================
;; UI 增强
;; ============================================================

;; 括号匹配高亮
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 回退显示字符美化
(defface fallback '((t :family "Fira Code Light"
                       :foreground "gray")) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))

;; Fringe 背景透明化
(set-face-attribute 'fringe nil :background nil)
(define-fringe-bitmap 'left-arrow [])
(define-fringe-bitmap 'left-curly-arrow [])
(define-fringe-bitmap 'left-triangle [])

;; 列号显示
(setq column-number-mode t)

;; 当前行高亮
(global-hl-line-mode t)

;; 快速终端访问
(global-set-key (kbd "<f9>") 'eshell)

;; ============================================================
;; 快捷键设置
;; ============================================================

(global-set-key (kbd "C-c f s") 'save-buffer)
(global-set-key (kbd "C-c w o") 'ace-window)
(global-set-key (kbd "C-c w w") 'delete-other-windows)
(global-set-key (kbd "C-c w 2") 'split-window-below)
(global-set-key (kbd "C-c w 3") 'split-window-right)
(global-set-key (kbd "C-c w q") 'delete-window)
(global-set-key (kbd "C-c b r") 'revert-buffer)
(global-set-key (kbd "C-c b p") 'projectile-ibuffer)

;; ============================================================
;; 剪贴板配置
;; ============================================================

(when (and (not (eq system-type 'darwin))
           (not (display-graphic-p)))
  (condition-case err
      (require 'my-clipboard)
    (error (message "剪贴板模块加载失败: %s" (error-message-string err))))
  (setq browse-url-browser-function 'nil))

;; macOS 终端下的剪贴板配置
(defun macos-terminal-clipboard-setup ()
  "Setup clipboard integration for terminal Emacs on macOS."
  (when (and (eq system-type 'darwin)
	     (not (display-graphic-p)))
    (setq interprogram-cut-function
	  (lambda (text &optional push)
	    "Copy TEXT to macOS clipboard using pbcopy."
	    (let ((process-connection-type nil))
	      (let ((proc (start-process "pbcopy" nil "pbcopy")))
		(process-send-string proc text)
		(process-send-eof proc)))))
    (setq interprogram-paste-function
	  (lambda ()
	    "Paste from macOS clipboard using pbpaste."
	    (shell-command-to-string "pbpaste")))
    (setq select-enable-clipboard t
	  save-interprogram-paste-before-kill t)))

(macos-terminal-clipboard-setup)

(provide 'pandaye-init)
;;; pandaye-init.el ends here
