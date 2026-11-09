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
      mouse-wheel-progressive-speed nil)

;; 剪贴板基础设置（GUI / 终端通用）
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      kill-ring-max 200)

;; 备份文件配置 - 禁用 ~ 后缀文件，保留自动保存
(setq make-backup-files nil)

;; ============================================================
;; UI 外观
;; ============================================================

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
(require 'hydration)

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
;; 光标颜色（终端 OSC 12 / 图形帧 cursor-color）
;; ============================================================

;; 光标颜色用来提示 meow 状态和输入法是否激活，两种显示环境都要接线：
;; `cursor-display' 内部按帧类型选择 OSC 12 还是 frame parameter，
;; 所以这里不再按 `display-graphic-p' 分叉。
(require 'cursor-display)
(add-hook 'meow-switch-state-hook #'cursor-display-update-cursor-color-for-state)
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
          #'cursor-display-update-cursor-color-for-buffer)
;; 图形界面下切换窗口焦点不触发 window-*-change-functions，需要单独补一次。
(add-hook 'after-focus-change-function
          #'cursor-display-update-cursor-color-for-buffer)

(provide 'init-editor)
;;; init-editor.el ends here
