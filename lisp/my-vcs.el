;;; my-vcs.el --- Version control configuration -*- lexical-binding: t; -*-

;; ============================================================
;; Git 与版本控制
;; ============================================================

(use-package magit
  :commands (magit-status magit-dispatch)
  :bind
  (("C-c j s" . magit-status)
   ("C-c j p" . magit-dispatch)))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode)
  (diff-hl-show-hunk-mouse-mode)
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#e9cd43"))))
  (diff-hl-insert ((t (:background "#03e94f"))))
  (diff-hl-delete ((t (:background "#f5597e")))))

(provide 'my-vcs)
;;; my-vcs.el ends here
