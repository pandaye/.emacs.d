;;; init-reader.el --- Feed, log, and translation readers -*- lexical-binding: t; -*-

(require 'translate)

(use-package gt
  :defer t
  :init
  (setq gt-langs my/gt-reading-langs
        gt-buffer-render-follow-p t))

(add-hook 'after-init-hook #'my/gt-pysbd-ensure-installed)

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
  (my/elfeed-apply-feeds)
  (define-key elfeed-show-mode-map (kbd "C-c k")
              #'my/elfeed-translate-dwim)
  (define-key elfeed-search-mode-map (kbd "C-c k")
              #'my/elfeed-translate-dwim))

(provide 'init-reader)
;;; my-reader.el ends here
