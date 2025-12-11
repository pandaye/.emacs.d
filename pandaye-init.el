;; -*- lexical-binding: t; -*-
(add-to-list 'exec-path "/opt/homebrew/bin/")
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(setq scroll-step 1
      scroll-conservatively 10000
      scroll-margin 0
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

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
  :bind
  (("C-c f p" . projectile-find-file)))

(use-package neotree
  :ensure t
  :bind
  ("C-c t p" . neotree-show)
  ("C-c t t" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char)))

(use-package rg
  :ensure t)

(defalias 'list-buffers 'ibuffer)

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
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-plus)             ; M-x 使用模糊匹配
                                (counsel-find-file . ivy--regex-plus)      ; 文件查找使用增强匹配
                                (counsel-file-jump . ivy--regex-plus)      ; 文件跳转使用模糊匹配
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
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode))

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-c j s") 'magit-status)
  (global-set-key (kbd "C-c j p") 'magit-dispatch-popup))

(use-package diff-hl
  :ensure t
  :init
  ;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
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

(require 'org-ssh)
(require 'my-org-writing)
(require 'my-word)

;;; Racket-mode configuration
;;; ==========================
(use-package paredit
  :ensure t
  :hook (racket-mode . paredit-mode))


(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode)
         ("\\.scrbl\\'" . racket-mode)) ; 支持 Scribble 文档
  :config
  ;; 确保 REPL 进程在后台运行，不会冻结 Emacs
  ;; (setq racket-program "path/to/your/racket") ; 如果 racket 不在系统 PATH 中，取消此行注释并设置路径
  (setq racket-run-in-background t)
  :hook
  (racket-mode . racket-xp-mode))

(use-package beancount
  :ensure t
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.bean\\'". beancount-mode)
  :config
  (define-key beancount-mode-map (kbd "TAB") nil))

(use-package go-mode
  :ensure t
  :mode
  ("\\.go\\'" . go-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(unless (featurep 'org-tempo)
  (require 'org-tempo))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (yas-reload-all)
  ;; (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package logview
  :ensure t)

;; Common Lisp Development Environment using SLIME
(use-package slime
  :ensure t
  :init
  ;; 让 SLIME 知道 Roswell 安装的 Lisp 在哪里
  (setq inferior-lisp-program "ros run")
  :mode
  ("\\.ros\\'" . lisp-mode)
  :config
  ;; slime-contrib 包含了很多非常有用的扩展，比如：
  ;; - slime-fancy-inspector: 更强大的对象检查器
  ;; - slime-tramp: 通过 TRAMP 连接到远程 Lisp 进程
  ;; - slime-xref-browser: 交叉引用浏览器 (谁调用了我？)
  (slime-setup '(slime-fancy slime-tramp slime-asdf slime-xref-browser))
  ;; 个人偏好：在 REPL 中让 Tab 键只做补全，不做缩进
  ;; (define-key slime-repl-mode-map (kbd "TAB") #'slime-complete-symbol)
  ;; 启用亚词级别的移动 (比如 a-long-variable-name 可以被看作 4 个词)
  (add-hook 'lisp-mode-hook (lambda () (subword-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda () (subword-mode 1))))

(use-package markdown-mode
  :ensure t)

(require 'my-lsp)

;; 快捷键设置，和 vscode 一致
(global-set-key (kbd "C-c f s") 'save-buffer)
(global-set-key (kbd "C-c w o") 'ace-window)
(global-set-key (kbd "C-c w 1") 'delete-other-windows)
(global-set-key (kbd "C-c w 2") 'split-window-below)
(global-set-key (kbd "C-c w 3") 'split-window-right)
(global-set-key (kbd "C-c w q") 'delete-window)
(global-set-key (kbd "C-c b r") 'revert-buffer)
(global-set-key (kbd "C-c b p") 'projectile-ibuffer)

(when (and (not (eq system-type 'darwin))
		   (not (display-graphic-p)))
  (require 'my-clipboard)
  (setq browse-url-browser-function 'nil))

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

