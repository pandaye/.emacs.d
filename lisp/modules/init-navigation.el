;;; init-navigation.el --- Navigation and project configuration -*- lexical-binding: t; -*-

;; ============================================================
;; 导航框架 - Vertico/Consult/Embark/Orderless
;; ============================================================

;; ============================================================
;; 项目与文件管理
;; ============================================================

(use-package projectile
  :defer 3
  :init
  (setq projectile-project-search-path '("~/Project/")
        projectile-completion-system 'auto)
  :config
  (projectile-mode 1))

(defun pandaye/navigation-dirvish-subtree-hide-total-line (readin dir)
  "Hide localized ls total line from Dirvish subtree READIN for DIR."
  ;; Dirvish currently strips the English "total used in directory" line in
  ;; `dirvish-subtree--readin', but GNU ls under a Chinese locale emits
  ;; "总计 ..." instead.  Keep this advice narrow so it only affects subtree
  ;; strings and can be removed if Dirvish handles localized totals upstream.
  (replace-regexp-in-string
   "\\`[[:space:]]*\\(total\\|总计\\)\\b[^\n]*\n"
   ""
   (funcall readin dir)))

(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --time-style=long-iso")
  :custom-face
  (dired-header ((t (:inherit shadow :weight normal))))
  :hook
  ((dired-mode . dired-hide-details-mode)))

(use-package dirvish
  :after dired
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-hide-details t)
  (dirvish-attributes '(vc-state subtree-state collapse file-size))
  (dirvish-subtree-state-style 'plus)
  (dirvish-use-header-line nil)
  (dirvish-use-mode-line nil)
  :custom-face
  (dirvish-hl-line ((t (:inherit hl-line :extend t))))
  (dirvish-hl-line-inactive ((t (:inherit hl-line :extend t))))
  (dirvish-subtree-state ((t (:inherit shadow :underline nil :background unspecified))))
  (dirvish-subtree-guide ((t (:inherit shadow :underline nil :background unspecified))))
  :config
  (unless (advice-member-p #'pandaye/navigation-dirvish-subtree-hide-total-line
                           'dirvish-subtree--readin)
    (advice-add 'dirvish-subtree--readin
                :around #'pandaye/navigation-dirvish-subtree-hide-total-line))
  :bind
  (:map dirvish-mode-map
        ("TAB" . dirvish-subtree-toggle)
        ("?" . dirvish-dispatch)
        ("a" . dirvish-setup-menu)
        ("s" . dirvish-quicksort)
        ("v" . dirvish-vc-menu)))

(defun pandaye/navigation-dired-project-root ()
  "Open Dired at the current project root."
  (interactive)
  (let ((dir (or (when (require 'projectile nil t)
                   (projectile-project-root))
                 default-directory)))
    (dired dir)))

(defalias 'my/dired-project-root #'pandaye/navigation-dired-project-root)

(use-package rg
  :defer t)

(defalias 'list-buffers 'ibuffer)

(provide 'init-navigation)
;;; init-navigation.el ends here
