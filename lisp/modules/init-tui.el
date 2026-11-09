;;; init-tui.el --- Terminal display configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; 终端（TTY）专属配置；图形界面侧见 `init-gui'。
;;
;; 既要支持 `emacs -nw' 直接启动，也要支持 `emacs --daemon' +
;; `emacsclient -nw'：配置加载时若已经处在终端里就立即应用，否则留给
;; `after-make-frame-functions'，等第一个终端帧出现时再应用。
;;
;; 终端的裁剪字形、OSC 剪贴板和 OSC 12 光标颜色都只在 TTY 有意义；
;; 这些设置大部分是全局量，同一个 daemon 里混用图形/终端客户端时以最后
;; 出现的帧为准。

;;; Code:

(require 'mwheel)

(condition-case err
    (require 'terminal-clipboard)
  (error (message "剪贴板模块加载失败: %s" (error-message-string err))))

(declare-function terminal-clipboard-pbcopy "terminal-clipboard" (text &optional push))
(declare-function terminal-clipboard-pbpaste "terminal-clipboard" ())
(declare-function terminal-clipboard-osc-52-cut-function "terminal-clipboard" (text &optional push))

(defcustom pandaye/tui-wheel-scroll-lines 1
  "终端里滚轮每格滚动的行数。"
  :type 'integer
  :group 'fringe)

;; display-table 里的截断/续行字形在终端下才看得到效果。
(defface fallback '((t :family "Fira Code Light"
                       :foreground "gray"))
  "Face for fallback display-table glyphs."
  :group 'faces)

(defun pandaye/tui--apply-display-table ()
  "Give truncated and wrapped lines nicer glyphs."
  (unless (char-table-p standard-display-table)
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback)))

(defun pandaye/tui--apply-clipboard ()
  "Route the clipboard through the terminal (OSC 52 or pbcopy/pbpaste)."
  (when (featurep 'terminal-clipboard)
    (if (eq system-type 'darwin)
        (setq interprogram-cut-function #'terminal-clipboard-pbcopy
              interprogram-paste-function #'terminal-clipboard-pbpaste)
      (setq interprogram-cut-function #'terminal-clipboard-osc-52-cut-function))
    (setq browse-url-browser-function nil)))

(defun pandaye/tui--apply-settings ()
  "Apply terminal-only display settings.
Idempotent: every terminal frame may call it again."
  ;; 终端下 fringe background 为 nil 会触发告警，显式设为 unspecified。
  (set-face-attribute 'fringe nil :background 'unspecified)
  (pandaye/tui--apply-display-table)
  ;; 终端里自动字符组合和 CJK 宽字符推断都更慢，关掉换取输入流畅。
  (setq-default auto-composition-mode nil)
  (setopt cjk-ambiguous-chars-are-wide nil)
  (setq mouse-wheel-scroll-amount (list pandaye/tui-wheel-scroll-lines))
  (pandaye/tui--apply-clipboard))

(defun pandaye/tui--apply-frame (&optional frame)
  "Apply terminal settings when FRAME is a terminal frame.
Returns non-nil when it applied, so it can be used directly in
`after-make-frame-functions'."
  (let ((frame (or frame (selected-frame))))
    (when (and (eq (framep frame) t)
               (not (display-graphic-p frame)))
      (pandaye/tui--apply-settings)
      t)))

(add-hook 'after-make-frame-functions #'pandaye/tui--apply-frame)

;; daemon 启动时还没有任何帧，等第一个客户端帧出现再决定用哪套配置。
(unless (or (daemonp) (display-graphic-p))
  (pandaye/tui--apply-settings))

(provide 'init-tui)
;;; init-tui.el ends here
