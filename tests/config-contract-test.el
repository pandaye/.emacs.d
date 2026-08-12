;;; config-contract-test.el --- Startup behavior contract -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for behavior which the structural refactor must keep.

;;; Code:

(require 'ert)

(defconst pandaye/test-global-keybindings
  '(("C-c f s" . save-buffer)
    ("C-c f r" . projectile-ripgrep)
    ("C-c f d" . my/common-dirs-find-file)
    ("C-c f p" . projectile-find-file)
    ("C-c f g" . consult-git-files)
    ("C-c f G" . consult-git-grep)
    ("C-c f f" . consult-find)
    ("C-c w o" . ace-window)
    ("C-c w w" . delete-other-windows)
    ("C-c w 2" . split-window-below)
    ("C-c w 3" . split-window-right)
    ("C-c w h" . windmove-left)
    ("C-c w l" . windmove-right)
    ("C-c w j" . windmove-down)
    ("C-c w k" . windmove-up)
    ("C-c w q" . delete-window)
    ("C-c b r" . revert-buffer)
    ("C-c b p" . projectile-ibuffer)
    ("C-c t p" . my/dired-project-root)
    ("C-c t t" . my/dired-project-root)
    ("C-c j s" . magit-status)
    ("C-c j p" . magit-dispatch)
    ("C-c o g" . gtd)
    ("C-c o t" . open-today-diary)
    ("C-c o d" . open-diary-by-date)
    ("C-c o l" . list-diary-files)
    ("C-c a" . org-agenda)
    ("C-c l c" . org-capture)
    ("C-c l l" . org-store-link)
    ("C-c l r" . org-clock-report)
    ("C-c i i" . toggle-input-method)
    ("C-c i j" . my/set-rime-jp)
    ("C-c i f" . my/set-rime-zh)
    ("C-c ." . my/rimel-toggle-ascii-punct)
    ("C-." . my/rimel-toggle-ascii-punct)
    ("C-c d d" . tmux-manager-switch-to-buffer)
    ("M-o" . my/hyperbole-action-key))
  "Global bindings preserved by the configuration refactor.")

(ert-deftest config-contract-preserves-global-keybindings ()
  (dolist (binding pandaye/test-global-keybindings)
    (should (eq (key-binding (kbd (car binding))) (cdr binding)))))

(ert-deftest config-contract-preserves-startup-modes ()
  (dolist (mode '(winner-mode global-hl-line-mode line-number-mode
                  column-number-mode minibuffer-depth-indicate-mode))
    (should (and (boundp mode) (symbol-value mode)))))

(ert-deftest config-contract-preserves-load-timing ()
  (dolist (feature '(my-lsp markdown-mode elfeed corfu meow))
    (should (featurep feature)))
  (dolist (feature '(gt logview org-roam slime projectile dirvish magit
                     diff-hl rimel))
    (should-not (featurep feature))))

(ert-deftest config-contract-preserves-hooks-and-advice ()
  (should (memq #'show-paren-mode prog-mode-hook))
  (should (memq #'my/gt-pysbd-ensure-installed after-init-hook))
  (should (memq #'my/org-agenda-colorize-category
                org-agenda-finalize-hook))
  (should (memq #'my/org-agenda-dim-block-separators
                org-agenda-finalize-hook))
  (should (advice-member-p #'my/refresh-cursor-color-after-input-method
                           'toggle-input-method)))

(provide 'config-contract-test)
;;; config-contract-test.el ends here
