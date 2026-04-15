;;; my-clipboard.el --- Unified clipboard for terminal Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; 终端 Emacs 剪贴板集成（GUI 无需此模块）。
;;
;; 策略：
;;   macOS 终端  → pbcopy / pbpaste（双向）
;;   其他终端    → OSC 52 复制到本地剪贴板（粘贴走 kill-ring）
;;
;; 加载方式：在 pandaye-init.el 中 (when (not (display-graphic-p)) (require 'my-clipboard))

;;; Code:

;; ── 工具函数 ──────────────────────────────────────────────

(defun my/in-tmux-p ()
  "Return non-nil when running inside tmux."
  (getenv "TMUX"))

;; ── macOS: pbcopy / pbpaste ──────────────────────────────

(defun my/pbcopy (text &optional _push)
  "Copy TEXT to macOS clipboard via pbcopy."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my/pbpaste ()
  "Return macOS clipboard content via pbpaste."
  (shell-command-to-string "pbpaste"))

;; ── 非 macOS: OSC 52 ────────────────────────────────────

(defun my/osc-52-copy (text)
  "Copy TEXT to system clipboard using OSC 52 escape sequence.
Works with modern terminals like WezTerm, iTerm2, and tmux."
  (let* ((utf8-text (encode-coding-string text 'utf-8 t))
         (encoded (base64-encode-string utf8-text t))
         (osc-seq (concat "\e]52;c;" encoded "\a")))
    (send-string-to-terminal osc-seq)))

(defun my/osc-52-cut-function (text &optional _push)
  "Copy TEXT to clipboard via OSC 52 in terminal.
Suitable as `interprogram-cut-function'."
  (when (and text (not (display-graphic-p)))
    (condition-case err
        (my/osc-52-copy text)
      (error
       (message "OSC 52 copy failed: %s" (error-message-string err))
       nil))))

;; ── 分派 ─────────────────────────────────────────────────

(cond
 ;; macOS 终端
 ((eq system-type 'darwin)
  (setq interprogram-cut-function   #'my/pbcopy
        interprogram-paste-function #'my/pbpaste))
 ;; 其他终端（Linux / remote SSH 等）
 (t
  (setq interprogram-cut-function #'my/osc-52-cut-function)))

;; 终端下通常没有合适的浏览器
(setq browse-url-browser-function nil)

;; ── 诊断 ─────────────────────────────────────────────────

(defun my/clipboard-info ()
  "Display clipboard configuration info."
  (interactive)
  (message "Clipboard: cut=%s, paste=%s, tmux=%s, system=%s"
           interprogram-cut-function
           interprogram-paste-function
           (if (my/in-tmux-p) "yes" "no")
           system-type))

(provide 'my-clipboard)
;;; my-clipboard.el ends here
