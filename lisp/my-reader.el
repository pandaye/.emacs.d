;;; my-reader.el --- Feed, log, and translation readers -*- lexical-binding: t; -*-

(require 'my-translate)

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

(provide 'my-reader)
;;; my-reader.el ends here
