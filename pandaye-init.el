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
;; 导航框架 - Vertico/Consult/Embark/Orderless
;; ============================================================

(setq enable-recursive-minibuffers t
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(minibuffer-depth-indicate-mode 1)

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200))

(use-package vertico
  :init
  (vertico-mode 1)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-sort-function #'vertico-sort-history-alpha)
  :custom-face
  (vertico-current ((t (:inherit hl-line
                        :foreground "#fdf4c1"
                        :weight bold
                        :extend t))))
  :bind
  ("C-c C-r" . vertico-repeat))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after recentf
  :bind
  (("C-c b b" . my/consult-buffer)
   ("C-x B" . consult-buffer-other-window)
   ("C-c f g" . consult-git-files)
   ("C-c f G" . consult-git-grep)
   ("C-c f f" . consult-find)
   ("C-s" . consult-line)
   ("C-r" . consult-line)
   ("M-y" . consult-yank-pop))
  :config
  (consult-customize
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   :preview-key nil)

  (defface my/consult-buffer-annotation
    '((t :inherit font-lock-comment-face :weight normal))
    "Face for custom `consult-buffer' annotations.")

  (defface my/consult-buffer-directory
    '((t :inherit font-lock-comment-face :weight normal))
    "Face for right-aligned buffer directory annotations.")

  (defface my/consult-buffer-virtual-file
    '((t :inherit shadow))
    "Face for unopened file candidates in `my/consult-buffer'.")

  (define-minor-mode my/consult-buffer-annotations-mode
    "Use custom annotations for `my/consult-buffer'."
    :global t
    :init-value t)

  (defvar my/consult-buffer--annotation-width 0
    "Precomputed annotation start column for `my/consult-buffer'.")

  (defvar my/consult-buffer-right-margin 1
    "Columns reserved at the right edge for `my/consult-buffer' annotations.")

  (defun my/consult-buffer--candidate-buffer (candidate)
    "Return buffer represented by Consult CANDIDATE metadata."
    (cond
     ((bufferp candidate) candidate)
     ((stringp candidate) (get-buffer candidate))))

  (defun my/consult-buffer--status (buffer)
    "Return short status string for BUFFER."
    (concat (if (buffer-modified-p buffer) "*" "-")
            (if (buffer-local-value 'buffer-read-only buffer) "%" "-")))

  (defun my/consult-buffer--mode-name (buffer)
    "Return display mode name for BUFFER."
    (with-current-buffer buffer
      (truncate-string-to-width
       (if (stringp mode-name)
           mode-name
         (format-mode-line mode-name))
       18 nil nil "...")))

  (defun my/consult-buffer--truncate-left (text width)
    "Truncate TEXT to WIDTH columns from the left."
    (cond
     ((<= width 0) "")
     ((<= (string-width text) width) text)
     (t
      (let* ((ellipsis "...")
             (ellipsis-width (string-width ellipsis)))
        (if (<= width ellipsis-width)
            (truncate-string-to-width ellipsis width)
          (nreverse
           (truncate-string-to-width (reverse text) width 0 nil ellipsis)))))))

  (defun my/consult-buffer--annotation-start (buffer)
    "Return Consult annotation start column for BUFFER."
    (min (my/consult-buffer--annotation-width-limit)
         (max my/consult-buffer--annotation-width
              (* (ceiling (string-width (buffer-name buffer))
                          consult--annotate-align-step)
                 consult--annotate-align-step))))

  (defun my/consult-buffer--annotation-width-limit ()
    "Return the maximum useful annotation start column."
    (max 0 (- (window-width (minibuffer-window))
              my/consult-buffer-right-margin
              (string-width " --  Lisp Interaction"))))

  (defun my/consult-buffer--source-width (source)
    "Return maximum visible candidate width in Consult SOURCE."
    (let ((width 0))
      (unless (or (plist-get source :hidden)
                  (plist-get source :async))
        (when-let* ((items (plist-get source :items)))
          (dolist (item (ignore-errors
                          (if (functionp items) (funcall items) items)))
            (let ((candidate (or (car-safe item) item)))
              (when (stringp candidate)
                (setq width
                      (max width
                           (string-width
                            (substring-no-properties candidate)))))))))
      width))

  (defun my/consult-buffer--compute-annotation-width (sources)
    "Return the annotation start column for initial Consult SOURCES."
    (let ((width 0))
      (dolist (source sources)
        (setq width
              (max width
                   (my/consult-buffer--source-width
                    (if (symbolp source) (symbol-value source) source)))))
      (min (my/consult-buffer--annotation-width-limit)
           (* (ceiling width consult--annotate-align-step)
              consult--annotate-align-step))))

  (defun my/consult-buffer--align-annotation (orig candidate annotation)
    "Use real spaces for stable `my/consult-buffer' annotations."
    (if (zerop my/consult-buffer--annotation-width)
        (funcall orig candidate annotation)
      (setq consult--annotate-align-width my/consult-buffer--annotation-width)
      (when annotation
        (let* ((candidate (if (fboundp 'consult--tofu-strip)
                              (consult--tofu-strip candidate)
                            (substring-no-properties candidate)))
               (padding (max 1 (- my/consult-buffer--annotation-width
                                   (string-width candidate)))))
          (concat (make-string padding ?\s) annotation)))))

  (defun my/consult-buffer--right-directory (buffer left directory)
    "Return DIRECTORY right-aligned after LEFT for BUFFER."
    (let* ((available (- (window-width (minibuffer-window))
                         (my/consult-buffer--annotation-start buffer)
                         my/consult-buffer-right-margin))
           (directory-width (max 0 (- available (string-width left))))
           (directory (my/consult-buffer--truncate-left directory directory-width))
           (padding (max 0 (- available
                               (string-width left)
                               (string-width directory)))))
      (concat
       (propertize (make-string padding ?\s) 'face 'my/consult-buffer-annotation)
       (propertize directory 'face 'my/consult-buffer-directory))))

  (defun my/consult-buffer-annotate (candidate)
    "Annotate buffer CANDIDATE with status, mode and right-aligned dirname."
    (when-let* ((buffer (my/consult-buffer--candidate-buffer candidate)))
      (let* ((file (buffer-file-name buffer))
             (directory (and file
                             (abbreviate-file-name
                              (file-name-directory file))))
             (left (format " %s  %-18s"
                           (my/consult-buffer--status buffer)
                           (my/consult-buffer--mode-name buffer))))
        (concat
         (propertize left 'face 'my/consult-buffer-annotation)
         (when directory
           (my/consult-buffer--right-directory buffer left directory))))))

  (defun my/consult-buffer--annotated-source (source)
    "Return Consult SOURCE with custom display settings."
    (let ((source (copy-sequence (if (symbolp source) (symbol-value source) source))))
      (setq source (plist-put source :name nil))
      (pcase (plist-get source :category)
        ('buffer
         (setq source (plist-put source :annotate #'my/consult-buffer-annotate)))
        ('file
         (setq source (plist-put source :face 'my/consult-buffer-virtual-file))))
      source))

  (defun my/consult-buffer--sources ()
    "Return `consult-buffer-sources' with custom buffer annotations."
    (if my/consult-buffer-annotations-mode
        (mapcar #'my/consult-buffer--annotated-source consult-buffer-sources)
      consult-buffer-sources))

  (defun my/consult-buffer ()
    "Run `consult-buffer' with custom buffer annotations."
    (interactive)
    (let* ((sources (my/consult-buffer--sources))
           (my/consult-buffer--annotation-width
            (my/consult-buffer--compute-annotation-width sources))
           (consult-preview-key nil))
      (consult-buffer sources)))

  (advice-add 'consult--annotate-align
              :around #'my/consult-buffer--align-annotation))

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

(defun my/hyperbole-assist-key ()
  "Run Hyperbole Assist Key from a regular key binding."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'hkey-either)))

(defun my/hyperbole-assist-help ()
  "Describe what Hyperbole Assist Key would do at point."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'hkey-help)))

(defun my/hyperbole-action-key ()
  "Run Hyperbole Action Key, loading Hyperbole on first use."
  (interactive)
  (require 'hyperbole)
  (call-interactively #'hkey-either))

(use-package hyperbole
  :commands (hyperbole hyperbole-mode hkey-either hkey-help)
  :bind (("C-c e h" . hyperbole)
         ("C-c e a" . hkey-either)
         ("C-c e s" . my/hyperbole-assist-key)
         ("C-c e ?" . hkey-help)
         ("C-c e S" . my/hyperbole-assist-help)
         ("M-RET" . my/hyperbole-action-key)))

(with-eval-after-load 'hycontrol
  (unless (display-graphic-p)
    (define-key hycontrol-windows-mode-map "j" nil)
    (define-key hycontrol-windows-mode-map "k" nil)
    (define-key hycontrol-windows-mode-map "i" nil)
    (define-key hycontrol-windows-mode-map "m" nil)))

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

(use-package clojure-mode)

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
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w k") #'windmove-up)
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
