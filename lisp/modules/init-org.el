;;; init-org.el --- Org writing and knowledge configuration -*- lexical-binding: t; -*-

;; ============================================================
;; Org 与写作系统
;; ============================================================

(require 'org-ssh)
(require 'tmux-manager)

(use-package htmlize
  :defer t)

(require 'static-blog)       ;; Org 静态博客发布

(unless (featurep 'org-tempo)
  (require 'org-tempo))

(use-package ox-gfm
  :after org)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c s c")
              #'org-ssh-connect-all-servers-in-file)
  (define-key org-mode-map (kbd "C-c s a")
              #'org-ssh-add-server-to-current-group)
  (define-key org-mode-map (kbd "C-c s s")
              #'org-ssh-show-ssh-config-summary)
  (define-key org-mode-map (kbd "C-c s n") #'org-ssh-create-template)
  (define-key org-mode-map (kbd "C-c s o") #'org-ssh-open-group-file)
  (define-key org-mode-map (kbd "C-c s l") #'org-ssh-create-link-and-open)
  (define-key org-mode-map (kbd "C-c s d") #'org-ssh-debug-command))

(with-eval-after-load 'hact
  (defact org-ssh-new ()
    "Create a new Org SSH link at point and open it in tmux."
    (call-interactively #'org-ssh-create-link-and-open)))

(provide 'init-org)
;;; init-org.el ends here
