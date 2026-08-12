;;; init-reader.el --- Feed, log, and translation readers -*- lexical-binding: t; -*-

(require 'translate)

(use-package gt
  :defer t
  :init
  (setq gt-langs translate-reading-langs
        gt-buffer-render-follow-p t))

(add-hook 'after-init-hook #'translate-pysbd-ensure-installed)

;; ============================================================
;; 日志与监控
;; ============================================================

(use-package logview
  :commands (logview-mode))

;; ============================================================
;; RSS 订阅
;; ============================================================
(defvaralias 'my/elfeed-feeds 'pandaye/reader-elfeed-feeds)

(defvar pandaye/reader-elfeed-feeds nil
  "User-local Elfeed subscriptions loaded from local vars.")

(defun pandaye/reader-elfeed-apply-feeds ()
  "Apply local or default Elfeed subscriptions to `elfeed-feeds'."
  (setq elfeed-feeds pandaye/reader-elfeed-feeds))

(defalias 'my/elfeed-apply-feeds #'pandaye/reader-elfeed-apply-feeds)

(use-package elfeed
  :config
  (pandaye/reader-elfeed-apply-feeds)
  (define-key elfeed-show-mode-map (kbd "C-c k")
              #'translate-elfeed-dwim)
  (define-key elfeed-search-mode-map (kbd "C-c k")
              #'translate-elfeed-dwim))

(provide 'init-reader)
;;; init-reader.el ends here
