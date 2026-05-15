;; -*- lexical-binding: t; -*-

;; ============================================================
;; 加载自定义常量
;; ============================================================
(require 'my-local-vars)
(require 'my-common-dirs)
;; ============================================================
;; 环境与基础设置
;; ============================================================

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin/"))

;; 这个是 Emacs 28 之后的内置语法检查工具，默认启用但不太好用，先禁用掉
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(setq scroll-step 1
      scroll-conservatively 10000
      scroll-margin 0
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)

;; CJK 和 emoji 宽字符设置（TUI 专属）
(setopt cjk-ambiguous-chars-are-wide nil)
(setq-default auto-composition-mode nil)

;; 剪贴板基础设置（GUI / 终端通用）
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      kill-ring-max 200)

;; 备份文件配置 - 禁用 ~ 后缀文件，保留自动保存
(setq make-backup-files nil)

;; ============================================================
;; GUI 专用配置
;; ============================================================

(when (display-graphic-p)
  (load "gui.el" :noerror))

;; ============================================================
;; UI 外观
;; ============================================================

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

;; 行号/列号显示（modeline 中 %l/%c 依赖这两个 mode 开启才会随光标实时刷新）
(line-number-mode 1)
(column-number-mode 1)

;; 当前行高亮
(global-hl-line-mode t)

;; 括号匹配高亮（所有编程模式）
(add-hook 'prog-mode-hook #'show-paren-mode)

;; 终端光标颜色（根据 Meow/Rime 状态动态变化）
;; TODO: GUI 也需要，但是目前没有使用到
(unless (display-graphic-p)
  (require 'my-cursor))

;; ============================================================
;; 基础工具 - 需要尽早加载
;; ============================================================

(condition-case err
    (require 'utils)
  (error (message "utils 加载失败: %s" (error-message-string err))))

(use-package try
  :commands (try))

(use-package which-key
  :defer 2
  :config (which-key-mode))

;; 安装 diminish 以支持 :diminish 关键字
(use-package diminish)

;; ============================================================
;; 导航框架 - Ivy/Counsel/Swiper
;; ============================================================

(use-package ivy
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
        ivy-initial-inputs-alist nil
		;; 对于 a.example 改成 a.ex 的场景，C-p/C-n 选择
		ivy-use-selectable-prompt t)
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-c b b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package counsel
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
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper-backward)))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; ============================================================
;; 项目与文件管理
;; ============================================================

(use-package projectile
  :defer 3
  :init
  (setq projectile-project-search-path '("~/Project/")
        projectile-completion-system 'auto)
  :config
  (projectile-mode 1)
  :bind
  (("C-c f p" . projectile-find-file)))

(use-package neotree
  :commands (neotree-show neotree-toggle neotree-find)
  :bind
  ("C-c t p" . neotree-show)
  ("C-c t t" . neotree-toggle)
  :config
  (setq neo-smart-open t
        neo-vc-integration '(face char)))

(use-package rg
  :defer t)

(defalias 'list-buffers 'ibuffer)

;; ============================================================
;; 窗口与编辑增强
;; ============================================================

(winner-mode 1)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-install-fonts))

(use-package ace-window
  :commands (ace-window)
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package rainbow-delimiters
  :hook (scheme-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode)
  (racket-mode . rainbow-delimiters-mode))

;; ============================================================
;; Git 与版本控制
;; ============================================================

(use-package magit
  :commands (magit-status magit-dispatch)
  :bind
  (("C-c j s" . magit-status)
   ("C-c j p" . magit-dispatch)))

(use-package diff-hl
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
;; Org 与写作系统
;; ============================================================

(condition-case err
    (progn
      (require 'org-ssh)
      (require 'tmux-manager))
  (error (message "可选模块加载失败: %s" (error-message-string err))))

(require 'my-org-writing)    ;; Org 外观美化
(require 'my-gtd)            ;; GTD 任务管理
(require 'my-rime)           ;; Rime 中文输入法
(require 'my-diary)          ;; 日记系统
(require 'my-org-roam)       ;; Org-roam 双向链接
(require 'my-translate)      ;; 阅读场景翻译

(unless (featurep 'org-tempo)
  (require 'org-tempo))

(use-package ox-gfm
  :after org)

;; ============================================================
;; 编程语言支持
;; ============================================================

(use-package paredit
  :hook
  (racket-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (clojure-mode . paredit-mode)
  (eval-expression-minibuffer-setup . paredit-mode)
  (ielm-mode . paredit-mode))

(use-package racket-mode
  :mode (("\\.rkt\\'" . racket-mode)
         ("\\.scrbl\\'" . racket-mode))
  :config
  (setq racket-run-in-background t)
  :hook
  (racket-mode . racket-xp-mode))

(use-package beancount
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.bean\\'" . beancount-mode)
  :config
  (define-key beancount-mode-map (kbd "TAB") nil))

(use-package go-mode
  :mode
  ("\\.go\\'" . go-mode))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package clojure-mode
  :hook ((clojure-mode . rainbow-delimiters-mode)))

(use-package cmake-mode
  :mode ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode))

(use-package slime
  :commands (slime)
  :init
  (setq inferior-lisp-program "ros run")
  :mode
  ("\\.ros\\'" . lisp-mode)
  :config
  (slime-setup '(slime-fancy slime-tramp slime-asdf slime-xref-browser))
  (add-hook 'lisp-mode-hook (lambda () (subword-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda () (subword-mode 1))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-fontify-code-block-default-mode 'fundamental-mode)
  :config
  (set-face-attribute 'markdown-code-face nil
                      :background "#32302f"
                      :extend t)
  (set-face-attribute 'markdown-pre-face nil
                      :background "#32302f"
                      :extend t)
  (set-face-attribute 'markdown-inline-code-face nil
                      :inherit '(font-lock-constant-face)
                      :background 'unspecified)
  (set-face-attribute 'markdown-language-keyword-face nil
                      :background "#32302f"
                      :foreground "gray35")
  (set-face-attribute 'markdown-language-info-face nil
                      :background "#32302f"
                      :foreground "gray35"))

;; ============================================================
;; Snippets
;; ============================================================

(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand))

(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================
;; LSP
;; ============================================================

(add-hook 'prog-mode-hook
          (lambda ()
            (unless (featurep 'my-lsp)
              (require 'my-lsp nil t))))

;; ============================================================
;; AI 工具
;; ============================================================

(add-to-list 'load-path (expand-file-name "lisp/org-opencode" user-emacs-directory))
(autoload 'org-opencode-mode "org-opencode" "Minor mode for opencode in Org buffers." t)

;; ============================================================
;; 日志与监控
;; ============================================================

(use-package logview
  :commands (logview-mode))

;; ============================================================
;; RSS 订阅
;; ============================================================
(defvar my/elfeed-feeds nil
  "User-local Elfeed subscriptions loaded from local vars.")

(defun my/elfeed-apply-feeds ()
  "Apply local or default Elfeed subscriptions to `elfeed-feeds'."
  (setq elfeed-feeds my/elfeed-feeds))

(use-package elfeed
  :config
  (my/elfeed-apply-feeds))
;; ============================================================
;; 快捷键
;; ============================================================

(global-set-key (kbd "C-c f s") #'save-buffer)
(global-set-key (kbd "C-c f r") #'projectile-ripgrep)
(global-set-key (kbd "C-c w o") #'ace-window)
(global-set-key (kbd "C-c w w") #'delete-other-windows)
(global-set-key (kbd "C-c w 2") #'split-window-below)
(global-set-key (kbd "C-c w 3") #'split-window-right)
(global-set-key (kbd "C-c w q") #'delete-window)
(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-c b p") #'projectile-ibuffer)

;; ============================================================
;; 终端剪贴板（终端统一由 my-clipboard 处理）
;; ============================================================

(unless (display-graphic-p)
  (condition-case err
      (require 'my-clipboard)
    (error (message "剪贴板模块加载失败: %s" (error-message-string err)))))

(provide 'pandaye-init)
;;; pandaye-init.el ends here
