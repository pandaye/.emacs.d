;;; init-editor.el --- Core editor configuration -*- lexical-binding: t; -*-

;; ============================================================
;; 加载自定义常量
;; ============================================================
(require 'common-dirs)
(require 'start-page)

(setq initial-buffer-choice #'start-page)
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
  (load "init-gui.el" :noerror))

;; ============================================================
;; UI 外观
;; ============================================================

;; 回退显示字符美化
(defface fallback '((t :family "Fira Code Light"
                       :foreground "gray"))
  "Face for fallback display-table glyphs."
  :group 'faces)
(unless (char-table-p standard-display-table)
  (setq standard-display-table (make-display-table)))
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

;; ============================================================
;; 基础工具 - 需要尽早加载
;; ============================================================

(require 'network-tools)

(use-package try
  :commands (try))

(use-package which-key
  :defer 2
  :config (which-key-mode))

;; 安装 diminish 以支持 :diminish 关键字
(use-package diminish)

;; ============================================================
;; 窗口与编辑增强
;; ============================================================

(winner-mode 1)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-install-fonts))

(use-package ace-window
  :commands (ace-window))

(defun pandaye/editor-hyperbole-action-key ()
  "Run Hyperbole Action Key, loading Hyperbole on first use."
  (interactive)
  (require 'hyperbole)
  (hyperbole-mode 1)
  (call-interactively #'hkey-either))

(defalias 'my/hyperbole-action-key #'pandaye/editor-hyperbole-action-key)

(use-package hyperbole
  :commands (hyperbole hyperbole-mode hkey-either hkey-help)
  :config
  (hkey-set-key (kbd "M-o") #'hkey-either))

;; ============================================================
;; 终端剪贴板（终端统一由 terminal-clipboard 处理）
;; ============================================================

(unless (display-graphic-p)
  (condition-case err
      (progn
        (require 'terminal-clipboard)
        (if (eq system-type 'darwin)
            (setq interprogram-cut-function #'terminal-clipboard-pbcopy
                  interprogram-paste-function #'terminal-clipboard-pbpaste)
          (setq interprogram-cut-function #'terminal-clipboard-osc-52-cut-function))
        (setq browse-url-browser-function nil))
    (error (message "剪贴板模块加载失败: %s" (error-message-string err)))))

;; Keep cursor wiring independent from `init-ui' error handling, matching the
;; legacy startup sequence where terminal cursor support loaded from editor.
(unless (display-graphic-p)
  (require 'cursor-display)
  (with-eval-after-load 'meow
    (add-hook 'meow-switch-state-hook #'cursor-display-update-cursor-color-for-state))
  (add-hook 'input-method-activate-hook #'cursor-display-update-cursor-color-for-buffer)
  (add-hook 'input-method-deactivate-hook #'cursor-display-update-cursor-color-for-buffer)
  (dolist (command '(activate-input-method deactivate-input-method
                     toggle-input-method))
    (unless (advice-member-p #'cursor-display-refresh-cursor-color-after-input-method
                             command)
      (advice-add command
                  :after #'cursor-display-refresh-cursor-color-after-input-method)))
  (add-hook 'window-buffer-change-functions
            #'cursor-display-update-cursor-color-for-buffer)
  (add-hook 'window-selection-change-functions
            #'cursor-display-update-cursor-color-for-buffer))

(provide 'init-editor)
;;; init-editor.el ends here
