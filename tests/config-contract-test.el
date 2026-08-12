;;; config-contract-test.el --- Startup behavior contract -*- lexical-binding: t; -*-

;;; Commentary:
;; Characterization tests for behavior which the structural refactor must keep.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst pandaye/test-global-keybindings
  '(("C-c f s" . save-buffer)
    ("C-c f r" . projectile-ripgrep)
    ("C-c f d" . common-dirs-find-file)
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
    ("C-c t p" . pandaye/navigation-dired-project-root)
    ("C-c t t" . pandaye/navigation-dired-project-root)
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
    ("C-c i j" . pandaye/input-set-rime-jp)
    ("C-c i f" . pandaye/input-set-rime-zh)
    ("C-c ." . pandaye/input-rimel-toggle-ascii-punct)
    ("C-." . pandaye/input-rimel-toggle-ascii-punct)
    ("C-c d d" . tmux-manager-switch-to-buffer)
    ("M-o" . pandaye/editor-hyperbole-action-key))
  "Global bindings preserved by the configuration refactor.")

(ert-deftest config-contract-preserves-global-keybindings ()
  (dolist (binding pandaye/test-global-keybindings)
    (should (eq (key-binding (kbd (car binding))) (cdr binding)))
    (should (commandp (cdr binding)))))

(ert-deftest config-contract-preserves-startup-modes ()
  (dolist (mode '(winner-mode global-hl-line-mode line-number-mode
                  column-number-mode minibuffer-depth-indicate-mode))
    (should (and (boundp mode) (symbol-value mode)))))

(ert-deftest config-contract-preserves-load-timing ()
  (dolist (feature '(init-lsp markdown-mode elfeed corfu meow))
    (should (featurep feature)))
  (dolist (feature '(gt logview org-roam slime projectile dirvish magit
                     diff-hl rimel))
    (should-not (featurep feature))))

(ert-deftest config-contract-preserves-hooks-and-advice ()
  (should (memq #'show-paren-mode prog-mode-hook))
  (dolist (hook '(scheme-mode-hook emacs-lisp-mode-hook lisp-mode-hook
                  racket-mode-hook clojure-mode-hook))
    (should (memq #'subtle-delimiter-mode (symbol-value hook))))
  (should (memq #'translate-pysbd-ensure-installed after-init-hook))
  (should (memq #'pandaye/org-agenda-colorize-category
                org-agenda-finalize-hook))
  (should (memq #'pandaye/org-agenda-dim-block-separators
                org-agenda-finalize-hook))
  (should (advice-member-p #'cursor-display-refresh-cursor-color-after-input-method
                           'toggle-input-method)))

(ert-deftest config-contract-keeps-public-symbol-aliases ()
  (dolist (names '((my/common-dirs-find-file . common-dirs-find-file)
                   (my/start-page . start-page)
                   (my/gt-translate-dwim . translate-translate-dwim)
                   (my/dired-project-root . pandaye/navigation-dired-project-root)
                   (my/set-rime-zh . pandaye/input-set-rime-zh)
                   (my/set-rimel-schema . pandaye/input-set-rimel-schema)
                   (my/org-ssh-insert-link-and-open . org-ssh-insert-link-and-open)
                   (my-static-blog-publish-file . static-blog-publish-file)
                   (my-subtle-delimiter-mode . subtle-delimiter-mode)
                   (my-markdown-match-italic-skip-intraword-underscore
                    . pandaye/markdown-match-italic-skip-intraword-underscore)
                   (my-slime-completion-at-point-if-connected
                    . pandaye/development-slime-completion-at-point-if-connected)))
    (should (eq (indirect-function (car names))
                (indirect-function (cdr names)))))
  (should (eq (indirect-variable 'my/common-dirs-alist)
              'common-dirs-alist))
  (should (eq (indirect-variable 'my/gt-wordbook-db-file)
              'translate-wordbook-db-file))
  (should (eq (indirect-variable 'my-static-blog-title)
              'static-blog-title))
  (should (eq (indirect-variable 'my-static-blog-public-directory)
              'static-blog-public-directory))
  (should (eq (indirect-variable 'my/gt-reading-langs)
              'translate-reading-langs))
  (should (eq (indirect-variable 'my/issue-file)
              'pandaye/org-issue-file))
  (should (eq (indirect-variable 'my/org-agenda-file-colors)
              'pandaye/org-agenda-file-colors)))

(ert-deftest config-contract-git-file-command-remains-callable ()
  (let (opened-file)
    (cl-letf (((symbol-function 'vc-git-root)
               (lambda (_directory) "/tmp/example-repository/"))
              ((symbol-function 'process-lines)
               (lambda (&rest _arguments) '("tracked-file.el")))
              ((symbol-function 'consult--read)
               (lambda (&rest _arguments) "tracked-file.el"))
              ((symbol-function 'consult--file-state) #'ignore)
              ((symbol-function 'find-file)
               (lambda (file) (setq opened-file file))))
      (pandaye/completion-consult-git-files)
      (should (equal opened-file
                     "/tmp/example-repository/tracked-file.el")))))

(provide 'config-contract-test)
;;; config-contract-test.el ends here
