;; -*- lexical-binding: t; -*-

(defvar *IS-MAC* (eq system-type 'darwin)
  "Check if the current system is macOS.")

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  ;; 终端下 fringe background 为 nil 会导致警告，设为 unspecified
  (set-face-attribute 'fringe nil :background 'unspecified))

;; 启用行号
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Org 在 major mode 初始化早期要求默认 `tab-width' 为 8；编程模式再局部使用 4。
(setq-default tab-width 8)
(add-hook 'prog-mode-hook (lambda () (setq-local tab-width 4)))

(defun pandaye/meow-execute-key (key-str)
  "Execute the command currently bound to KEY-STR.
This acts as a universal bridge for Meow leader keys to emulate
standard Emacs keybindings, respecting the current mode's keymap."
  (let ((command (key-binding (kbd key-str))))
    (if command
        (progn
          (setq-local this-command command) ; Make the command known to Emacs
          (call-interactively command))
      (message "No command is bound to %s in the current context" key-str))))

;; Define a set of reusable bridge functions
(defun pandaye/meow-C-left ()
  "Execute the command for <C-left>."
  (interactive)
  (pandaye/meow-execute-key "<C-left>"))

(defun pandaye/meow-C-right ()
  "Execute the command for <C-right>."
  (interactive)
  (pandaye/meow-execute-key "<C-right>"))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("s" . "C-s") ; leader+s 映射到搜索功能
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("p h" . ("C-left" . pandaye/meow-C-left))
   '("p l" . ("C-right" . pandaye/meow-C-right)))
  (meow-motion-overwrite-define-key
   '("<escape>" . ignore))
  (meow-normal-define-key
   '(";" . meow-reverse)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("C" . meow-change)
   '("D" . meow-kill)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("M" . set-mark-command)
   '("n" . meow-search)
   '("N" . meow-pop-or-unpop-to-mark)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("P" . meow-yank)
   '("Q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-line)
   '("S" . meow-goto-line)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-to-global-mark)
   '("'" . repeat)
   '("<escape>" . ignore)))

;; 1. 定义 inner 函数：返回不含首尾空白的行
(defun my/meow--inner-of-line-trimmed ()
  "Return bounds of line without leading/trailing whitespace."
  (cons (save-excursion
          (beginning-of-line)
          (skip-chars-forward " \t")
          (point))
        (save-excursion
          (end-of-line)
          (skip-chars-backward " \t")
          (point))))

;; 2. 定义 bounds 函数：返回包含换行符的版本（可选）
(defun my/meow--bounds-of-line-trimmed ()
  "Return bounds of line without leading/trailing whitespace, but include newline."
  (cons (save-excursion
          (beginning-of-line)
          (skip-chars-forward " \t")
          (point))
        (save-excursion
          (end-of-line)
          (skip-chars-backward " \t")
          (min (1+ (point)) (point-max)))))

(defun meow-not-insert-p ()
  "Return t if Meow is not in insert state, nil if in insert state."
  (not (and (bound-and-true-p meow-mode)
            (eq (meow--current-state) 'insert))))

(use-package meow
  :vc (:url "https://github.com/meow-edit/meow")
  :demand t
  :config
  ;; 注册自定义 thing
  (meow-thing-register 'line-trimmed
                       'my/meow--inner-of-line-trimmed
                       'my/meow--bounds-of-line-trimmed)
  (add-to-list 'meow-char-thing-table '(?t . line-trimmed))
  (add-to-list 'meow-mode-state-list '(my/org-list-mode . motion))
  (meow-setup)
  (meow-global-mode 1))

;; 自定义 Modeline - 简洁实用的状态栏

;; ── Faces ────────────────────────────────────────────────

(defface custom-modeline-meow-normal
  '((t :foreground "#282828" :background "#83a598" :weight bold))
  "Face for Meow normal state."
  :group 'faces)

(defface custom-modeline-meow-motion
  '((t :foreground "#282828" :background "#fe8019" :weight bold))
  "Face for Meow motion state."
  :group 'faces)

(defface custom-modeline-meow-insert
  '((t :foreground "#282828" :background "#fb4934" :weight bold))
  "Face for Meow insert state."
  :group 'faces)

(defface custom-modeline-meow-keypad
  '((t :foreground "#282828" :background "#d3869b" :weight bold))
  "Face for Meow keypad state."
  :group 'faces)

(defface custom-modeline-meow-beacon
  '((t :foreground "#282828" :background "#b16286" :weight bold))
  "Face for Meow beacon state."
  :group 'faces)

(defface custom-modeline-buffer-modified
  '((t :foreground "#fe8019" :weight bold))
  "Face for modified buffer indicator."
  :group 'faces)

(defface custom-modeline-git-branch
  '((t :foreground "#b8bb26" :weight normal))
  "Face for git branch name."
  :group 'faces)

(defface custom-modeline-separator
  '((t :foreground "#665c54" :weight normal))
  "Face for modeline separators."
  :group 'faces)

(defface custom-modeline-major-mode-face
  '((t :inherit font-lock-keyword-face :weight bold :foreground "orange"))
  "Face for the major mode name in the custom modeline."
  :group 'faces)

(defface custom-projectile-name-face
  '((t :inherit font-lock-keyword-face :foreground "grey"))
  "Face for the projectile project name in the custom modeline."
  :group 'faces)

;; ── 段落函数 ─────────────────────────────────────────────

(defun custom-modeline-meow-state ()
  "Return formatted Meow state for modeline."
  (when (and (fboundp 'meow--current-state) (bound-and-true-p meow-mode))
    (let ((state (meow--current-state)))
      (pcase state
        ('normal (propertize "  N  " 'face 'custom-modeline-meow-normal))
        ('insert (propertize "  I  " 'face 'custom-modeline-meow-insert))
        ('motion (propertize "  M  " 'face 'custom-modeline-meow-motion))
        ('keypad (propertize "  K  " 'face 'custom-modeline-meow-keypad))
        ('beacon (propertize "  B  " 'face 'custom-modeline-meow-beacon))
        (_       (propertize "  ?  " 'face 'font-lock-warning-face))))))

(defun custom-modeline-buffer-status ()
  "Return buffer modification status."
  (cond
   (buffer-read-only (propertize "RO" 'face 'font-lock-warning-face))
   ((buffer-modified-p)
    (propertize "●" 'face 'custom-modeline-buffer-modified))
   (t (propertize "-" 'face 'custom-modeline-separator))))

(defun custom-modeline-git-branch ()
  "Return current git branch via `vc-mode' (Emacs 内置，无额外进程开销)."
  (when (and vc-mode (stringp vc-mode))
    ;; vc-mode 格式如 " Git:main" 或 " Git-main"，去掉前缀
    (let ((branch (replace-regexp-in-string "^ Git[:-]" "" vc-mode)))
      (propertize (format "⎇ %s" branch) 'face 'custom-modeline-git-branch))))

(defun custom-modeline-buffer-name ()
  "Return formatted buffer name with status-based coloring."
  (let ((name (buffer-name)))
    (cond
     ;; 只读
     (buffer-read-only
      (propertize name 'face '(:foreground "#fe8019" :weight bold)))
     ;; 已修改
     ((buffer-modified-p)
      (propertize name 'face '(:foreground "#fb4934" :weight bold)))
     ;; Git 跟踪（借助 vc-mode，Emacs 自行维护，无额外进程）
     ((and vc-mode (stringp vc-mode))
      (propertize name 'face '(:foreground "#b8bb26" :weight normal)))
     ;; 普通
     (t (propertize name 'face 'mode-line-buffer-id)))))

(defun custom-modeline-major-mode ()
  "Return formatted major mode, handling both string and list `mode-name`."
  (let ((name (if (listp mode-name)
                  (car mode-name)
                mode-name)))
    (when (stringp name)
      (propertize name 'face 'custom-modeline-major-mode-face))))

(defun custom-modeline-position ()
  "Return cursor position info (line:col).
Uses `%l'/`%c' format specifiers so Emacs redisplay engine refreshes
them after every command (requires `line-number-mode' and
`column-number-mode' to be enabled)."
  (propertize "%l:%c" 'face 'font-lock-type-face))

(defun custom-modeline-separator ()
  "Return a separator."
  (propertize " | " 'face 'custom-modeline-separator))

;; ── mode-line-format ─────────────────────────────────────

(setq-default mode-line-format
  '(;; 左侧：Meow 状态
    (:eval (let ((state (custom-modeline-meow-state)))
             (when state (concat state))))
    " "
    ;; Projectile 项目名
    (:eval (when (featurep 'projectile)
             (propertize (format "[%s]" (projectile-project-name))
                         'face 'custom-projectile-name-face)))
    " "
    ;; Buffer 名称（带状态着色）
    (:eval (custom-modeline-buffer-name))
    " "
    ;; Major mode
    (:eval (custom-modeline-major-mode))
    " "
    ;; Git branch
    (:eval (let ((git-str (custom-modeline-git-branch)))
             (if git-str (concat " " git-str) "")))
    " "
    mode-line-misc-info
    ;; 中间弹性空白，将右侧推到最右
    (:eval
     (let* ((pos-str (format-mode-line '(:eval (custom-modeline-position))))
            (pct-str (format-mode-line "%p"))
            (mule-str (format-mode-line mode-line-mule-info))
            (right-width (+ (string-width pos-str)
                            (string-width pct-str)
                            (string-width mule-str)
                            5)))
       (propertize " " 'display `(space :align-to (- right ,right-width)))))
    ;; 右侧：位置 → 百分比 → 编码
    (:eval (custom-modeline-position))
    " "
    "[" (:eval (format-mode-line "%p")) "]"
    " "
    mode-line-mule-info))

;; 设置 modeline 高度和外观
(set-face-attribute 'mode-line nil
                    :height 100
                    :box '(:line-width 1 :color "#504945"))
(set-face-attribute 'mode-line-inactive nil
                    :height 100
                    :box '(:line-width 1 :color "#3c3836"))


(provide 'my-ui-keyboard)
