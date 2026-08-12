;; -*- lexical-binding: t; -*-

;; ============================================================
;; 加载自定义常量
;; ============================================================
(require 'my-local-vars)
(require 'my-common-dirs)
(require 'my-start-page)

(setq initial-buffer-choice #'my/start-page)
;; ============================================================
;; 环境与基础设置
;; ============================================================

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin/"))

;; 默认文字从左到右显示，禁用双向文本算法（BPA）以提升性能
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; 输入时跳过字体化
(setq redisplay-skip-fontification-on-input t)
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

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

(require 'utils)

(use-package try
  :commands (try))

(use-package which-key
  :defer 2
  :config (which-key-mode))

;; 安装 diminish 以支持 :diminish 关键字
(use-package diminish)

;; ============================================================
;; 导航框架 - Vertico/Consult/Embark/Orderless
;; ============================================================

(require 'my-completion)

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

(defun my/dirvish-subtree-hide-total-line (readin dir)
  "Hide localized ls total line from Dirvish subtree READIN for DIR."
  ;; Dirvish currently strips the English "total used in directory" line in
  ;; `dirvish-subtree--readin', but GNU ls under a Chinese locale emits
  ;; "总计 ..." instead.  Keep this advice narrow so it only affects subtree
  ;; strings and can be removed if Dirvish handles localized totals upstream.
  (replace-regexp-in-string
   "\\`[[:space:]]*\\(total\\|总计\\)\\b[^\n]*\n"
   ""
   (funcall readin dir)))

(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --time-style=long-iso")
  :custom-face
  (dired-header ((t (:inherit shadow :weight normal))))
  :hook
  ((dired-mode . dired-hide-details-mode)))

(use-package dirvish
  :after dired
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-hide-details t)
  (dirvish-attributes '(vc-state subtree-state collapse file-size))
  (dirvish-subtree-state-style 'plus)
  (dirvish-use-header-line nil)
  (dirvish-use-mode-line nil)
  :custom-face
  (dirvish-hl-line ((t (:inherit hl-line :extend t))))
  (dirvish-hl-line-inactive ((t (:inherit hl-line :extend t))))
  (dirvish-subtree-state ((t (:inherit shadow :underline nil :background unspecified))))
  (dirvish-subtree-guide ((t (:inherit shadow :underline nil :background unspecified))))
  :config
  (advice-add 'dirvish-subtree--readin
              :around #'my/dirvish-subtree-hide-total-line)
  :bind
  (:map dirvish-mode-map
        ("TAB" . dirvish-subtree-toggle)
        ("?" . dirvish-dispatch)
        ("a" . dirvish-setup-menu)
        ("s" . dirvish-quicksort)
        ("v" . dirvish-vc-menu)))

(defun my/dired-project-root ()
  "Open Dired at the current project root."
  (interactive)
  (let ((dir (or (when (require 'projectile nil t)
                   (projectile-project-root))
                 default-directory)))
    (dired dir)))

(global-set-key (kbd "C-c t p") #'my/dired-project-root)
(global-set-key (kbd "C-c t t") #'my/dired-project-root)

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

(defun my/hyperbole-action-key ()
  "Run Hyperbole Action Key, loading Hyperbole on first use."
  (interactive)
  (require 'hyperbole)
  (hyperbole-mode 1)
  (call-interactively #'hkey-either))

(use-package hyperbole
  :commands (hyperbole hyperbole-mode hkey-either hkey-help)
  :bind (("C-c e h" . hyperbole)
         ("C-c e a" . hkey-either)
         ("C-c e ?" . hkey-help))
  :config
  (hkey-set-key (kbd "M-o") #'hkey-either))

(require 'my-subtle-delimiter)



;; ============================================================
;; Git 与版本控制
;; ============================================================

(use-package magit
  :commands (magit-status magit-dispatch)
  :bind
  (("C-c j s" . magit-status)
   ("C-c j p" . magit-dispatch)))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-show-hunk-mouse-mode)
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#e9cd43"))))
  (diff-hl-insert ((t (:background "#03e94f"))))
  (diff-hl-delete ((t (:background "#f5597e")))))

;; ============================================================
;; Org 与写作系统
;; ============================================================

(require 'org-ssh)
(require 'tmux-manager)

(use-package htmlize
  :defer t)

(require 'my-org-writing)    ;; Org 外观美化
(require 'my-static-blog)     ;; Org 静态博客发布
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

(declare-function paredit-mode "paredit")
(defvar paredit-mode-map)

(defun my/eval-expression-paredit-setup ()
  "Enable Paredit in `eval-expression' without stealing RET."
  (require 'paredit)
  (paredit-mode 1)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map paredit-mode-map)
    (define-key map (kbd "RET") #'exit-minibuffer)
    (define-key map (kbd "<return>") #'exit-minibuffer)
    (define-key map "\C-m" #'exit-minibuffer)
    (setq-local minor-mode-overriding-map-alist
                (assq-delete-all 'paredit-mode minor-mode-overriding-map-alist))
    (push `(paredit-mode . ,map) minor-mode-overriding-map-alist)))

(use-package paredit
  :hook
  (racket-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (clojure-mode . paredit-mode)
  (eval-expression-minibuffer-setup . my/eval-expression-paredit-setup)
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

(use-package clojure-mode)

(use-package cmake-mode
  :mode ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt))

(use-package corfu-terminal
  :if (< emacs-major-version 31)
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

(defun my-common-lisp-completion-setup ()
  "Use Corfu for Common Lisp completion without lsp-bridge conflict."
  (when (and (fboundp 'lsp-bridge-mode)
             (bound-and-true-p lsp-bridge-mode))
    (lsp-bridge-mode -1))
  (when (fboundp 'slime--completion-at-point)
    (remove-hook 'completion-at-point-functions #'slime--completion-at-point t)
    (add-hook 'completion-at-point-functions
              #'my-slime-completion-at-point-if-connected nil t))
  (corfu-mode 1))

(defun my-slime-completion-at-point-if-connected ()
  "Complete with SLIME when connected or auto-start is enabled."
  (when (and (fboundp 'slime-connected-p)
             (or (slime-connected-p)
                 (not (eq slime-auto-start 'never))))
    (slime--completion-at-point)))

(add-hook 'lisp-mode-hook #'my-common-lisp-completion-setup)
(add-hook 'slime-mode-hook #'my-common-lisp-completion-setup)
(add-hook 'slime-repl-mode-hook #'my-common-lisp-completion-setup)

(use-package slime
  :commands (slime)
  :init
  (setq inferior-lisp-program "ros run")
  (setq slime-auto-start 'always)
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
  (defun my-markdown-match-italic-skip-intraword-underscore (orig-fun last)
    "Make `markdown-mode' ignore intraword underscore emphasis like GFM."
    (let (found done)
      (while (and (not done) (funcall orig-fun last))
        (if (and (not (derived-mode-p 'gfm-mode))
                 (eq (char-after (match-beginning 0)) ?_)
                 (not (markdown--gfm-markup-underscore-p
                       (match-beginning 0)
                       (match-end 3))))
            (progn
              (goto-char (min (1+ (match-beginning 0)) last))
              (unless (< (point) last)
                (setq done t)))
          (setq found t
                done t)))
      found))

  (unless (advice-member-p #'my-markdown-match-italic-skip-intraword-underscore
                           'markdown-match-italic)
    (advice-add 'markdown-match-italic
                :around #'my-markdown-match-italic-skip-intraword-underscore))

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

(require 'my-gptel)
;; (require 'my-agent-shell)

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
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w k") #'windmove-up)
(global-set-key (kbd "C-c w q") #'delete-window)

(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-c b p") #'projectile-ibuffer)

(global-set-key (kbd "M-o") #'my/hyperbole-action-key)

;; ============================================================
;; 终端剪贴板（终端统一由 my-clipboard 处理）
;; ============================================================

(unless (display-graphic-p)
  (condition-case err
      (require 'my-clipboard)
    (error (message "剪贴板模块加载失败: %s" (error-message-string err)))))

(provide 'pandaye-init)
;;; pandaye-init.el ends here
