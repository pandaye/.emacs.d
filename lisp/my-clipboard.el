;;; my-clipboard.el --- Clipboard configuration for remote Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; 配置远程 Shell 中的 Emacs 剪贴板功能
;; 支持两种场景：
;; 1. 通过 OSC 52 复制到本地剪贴板（需要终端支持）
;; 2. 在远程服务器内部使用 Emacs kill-ring（Ctrl-y 粘贴）

;;; Code:

;; ==================
;; 基础配置
;; ==================

;; 启用 X 剪贴板支持（如果可用）
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; 保存剪贴板历史
(setq save-interprogram-paste-before-kill t)

;; 增大 kill-ring 容量
(setq kill-ring-max 200)

;; ==================
;; OSC 52 支持
;; ==================
;; OSC 52 允许通过终端转义序列复制到本地剪贴板
;; 支持的终端：iTerm2, WezTerm, Alacritty, tmux (需配置)

(defun my/osc-52-copy (text)
  "Copy TEXT to system clipboard using OSC 52 escape sequence.
Works with modern terminals like WezTerm, iTerm2, and tmux."
  (let* ((utf8-text (encode-coding-string text 'utf-8 t))
         (encoded (base64-encode-string utf8-text t))
         (osc-seq (concat "\e]52;c;" encoded "\a")))
    (send-string-to-terminal osc-seq)))

(defun my/copy-to-clipboard (text &optional _push)
  "Copy TEXT to clipboard using OSC 52 if in terminal.
This function can be used as `interprogram-cut-function'."
  (when (and text (not (display-graphic-p)))
    (condition-case err
        (my/osc-52-copy text)
      (error 
       (message "OSC 52 copy failed: %s" (error-message-string err))
       nil))))

;; 设置 Emacs 使用 OSC 52 复制到系统剪贴板
(setq interprogram-cut-function #'my/copy-to-clipboard)

(defun my/clipboard-info ()
  "Display clipboard configuration info."
  (interactive)
  (message "Clipboard: OSC52=%s, Tmux=%s, Display=%s"
           (if (fboundp 'my/osc-52-copy) "enabled" "disabled")
           (if (my/in-tmux-p) "yes" "no")
           (if (display-graphic-p) "GUI" "Terminal")))

(provide 'my-clipboard)
;;; my-clipboard.el ends here
