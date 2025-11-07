;; -*- lexical-binding: t; -*-
(add-to-list 'load-path
			 (expand-file-name "meow" user-emacs-directory))

(defvar *IS-MAC* (eq system-type 'darwin)
  "Check if the current system is macOS.")

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

;; 启用行号
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default tab-width 4)

(require 'meow)

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
  (meow-motion-define-key
   '("y" . meow-save)
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
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
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
   ;; '("q" . meow-quit)
   '("'" . repeat)
   '("<escape>" . ignore)))

(meow-setup)
(meow-global-mode 1)

(defun meow-not-insert-p ()
  "Return t if Meow is not in insert state, nil if in insert state."
  (not (and (bound-and-true-p meow-mode)
            (eq (meow--current-state) 'insert))))

;; 自定义 Modeline - 简洁实用的状态栏
(defface custom-modeline-meow-normal
  '((t :foreground "#83a598" :weight bold))
  "Face for Meow normal state.")

(defface custom-modeline-meow-motion
  '((t :foreground "#8ec07c" :weight bold))
  "Face for Meow motion state.")

(defface custom-modeline-meow-insert
  '((t :foreground "#fb4934" :weight bold))
  "Face for Meow insert state.")

(defface custom-modeline-meow-keypad
  '((t :foreground "#d3869b" :weight bold))
  "Face for Meow keypad state.")

(defface custom-modeline-meow-visual
  '((t :foreground "#fabd2f" :weight bold))
  "Face for Meow visual state.")

(defface custom-modeline-buffer-modified
  '((t :foreground "#fe8019" :weight bold))
  "Face for modified buffer indicator.")

(defface custom-modeline-git-branch
  '((t :foreground "#b8bb26" :weight normal))
  "Face for git branch name.")

(defface custom-modeline-separator
  '((t :foreground "#665c54" :weight normal))
  "Face for modeline separators.")

;; 缓存变量，提高性能
(defvar custom-modeline-git-branch-cache nil)
(defvar custom-modeline-git-branch-cache-file nil)
(defvar custom-modeline-buffer-git-status-cache nil)
(defvar custom-modeline-buffer-git-status-cache-file nil)

(defun custom-modeline-meow-state ()
  "Return formatted Meow state for modeline."
  (when (and (fboundp 'meow--current-state) (bound-and-true-p meow-mode))
    (let ((state (meow--current-state)))
      (pcase state
        ('normal (propertize "N" 'face 'custom-modeline-meow-normal))
        ('insert (propertize "I" 'face 'custom-modeline-meow-insert))
        ('motion (propertize "M" 'face 'custom-modeline-meow-motion))
        ('keypad (propertize "K" 'face 'custom-modeline-meow-keypad))
        (_ (propertize "?" 'face 'custom-modeline-meow-visual))))))

(defun custom-modeline-buffer-status ()
  "Return buffer modification status."
  (cond
   (buffer-read-only (propertize "RO" 'face 'font-lock-warning-face))
   ((buffer-modified-p) 
    (propertize "●" 'face 'custom-modeline-buffer-modified))
   (t (propertize "-" 'face 'custom-modeline-separator))))

(defun custom-modeline-git-branch ()
  "Return current git branch if available with caching."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let ((current-file buffer-file-name))
      ;; 只有当文件改变时才重新获取 git 信息
      (when (or (not custom-modeline-git-branch-cache-file)
                (not (string= current-file custom-modeline-git-branch-cache-file)))
        (setq custom-modeline-git-branch-cache-file current-file)
        (setq custom-modeline-git-branch-cache
              (condition-case nil
                (when (vc-git-registered current-file)
                  (let ((branch (vc-git--symbolic-ref current-file)))
                    (when branch
                      (propertize (format "⎇ %s" branch) 
                                 'face 'custom-modeline-git-branch))))
                (error nil))))
      custom-modeline-git-branch-cache)))

(defun custom-modeline-buffer-name ()
  "Return formatted buffer name with status-based coloring."
  (let ((name (buffer-name))
        (current-file buffer-file-name))
    (cond
     ;; 只读文件 - 橙色
     (buffer-read-only 
      (propertize name 'face '(:foreground "#fe8019" :weight bold)))
     ;; 修改过的文件 - 红色
     ((buffer-modified-p) 
      (propertize name 'face '(:foreground "#fb4934" :weight bold)))
     ;; Git 跟踪的文件 - 绿色（带缓存）
     ((and current-file 
           (or (and custom-modeline-buffer-git-status-cache-file
                    (string= current-file custom-modeline-buffer-git-status-cache-file)
                    custom-modeline-buffer-git-status-cache)
               (progn
                 (setq custom-modeline-buffer-git-status-cache-file current-file)
                 (setq custom-modeline-buffer-git-status-cache
                       (condition-case nil
                           (vc-git-registered current-file)
                         (error nil)))
                 custom-modeline-buffer-git-status-cache)))
      (propertize name 'face '(:foreground "#b8bb26" :weight normal)))
     ;; 普通文件 - 默认颜色
     (t (propertize name 'face 'mode-line-buffer-id)))))

(defface custom-modeline-major-mode-face
  '((t :inherit font-lock-keyword-face :weight bold :foreground "orange"))
  "Face for the major mode name in the custom modeline."
  :group 'faces)

(defface custom-projectile-name-face
  '((t :inherit font-lock-keyword-face :foreground "grey"))
  "Face for the major mode name in the custom modeline."
  :group 'faces)


(defun custom-modeline-major-mode ()
  "Return formatted major mode, handling both string and list `mode-name`."
  (let ((name (if (listp mode-name)
                  (car mode-name)
                mode-name)))
    ;; 确保我们只 propertize 字符串
    (when (stringp name)
      (propertize name 'face 'custom-modeline-major-mode-face))))

(defun custom-modeline-position ()
  "Return cursor position info."
  (propertize (concat (format "%d:%d" (line-number-at-pos) (current-column))
					  " [%p]")
              'face 'font-lock-type-face))

(defun custom-modeline-separator ()
  "Return a separator."
  (propertize " | " 'face 'custom-modeline-separator))

(defun projectile-mode-line ()
  '(:eval (format " Proj[%s]" (projectile-project-name))))

(setq-default mode-line-format
			  '(;; 左侧信息
				(:eval (when (custom-modeline-meow-state)
						 (concat " " (custom-modeline-meow-state) " ")))
				" "
				(:eval (when (featurep 'projectile)
						 (propertize (format "Proj[%s]" (projectile-project-name)) 'face 'custom-projectile-name-face)))
				" "
				(:eval (custom-modeline-buffer-name))
				" "
				(:eval (custom-modeline-major-mode))
				" "
				(:eval (let ((git-str (custom-modeline-git-branch)))
						 (if git-str (concat " " git-str) "")))
				" "
				mode-line-misc-info
				
				;; 中间填充：根据右侧要显示的项长度计算空格，使右侧真正贴到最右
				(:eval
				 (let* ((mule-str (format-mode-line mode-line-mule-info))
						(pos-str  (format-mode-line (custom-modeline-position)))
						;; 用 string-width 更准确地计算显示宽度（对中文/宽字符友好）
						(right-width (+ (string-width mule-str)
										(string-width pos-str)
										3))) ;; 预留的空白（可以调整）
				   (propertize " " 'display `(space :align-to (- right ,right-width)))))

				;; 右侧信息（按顺序显示：位置 -> 间隔 -> 输入法/编码信息）
				(:eval (custom-modeline-position))
				" "
				mode-line-mule-info))

;; 设置 modeline 高度和外观
(set-face-attribute 'mode-line nil
                    :height 100
                    :box '(:line-width 1 :color "#504945"))
(set-face-attribute 'mode-line-inactive nil
                    :height 100
                    :box '(:line-width 1 :color "#3c3836"))

;; 优化刷新性能 - 添加 hook 来确保实时更新
(defun custom-modeline-update ()
  "Force update modeline."
  (force-mode-line-update))

;; 在状态改变时立即更新 modeline
(add-hook 'post-command-hook #'custom-modeline-update)
(add-hook 'buffer-list-update-hook #'custom-modeline-update)

(provide 'my-ui-keyboard)
