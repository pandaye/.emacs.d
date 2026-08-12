;;; init-development.el --- Programming environment configuration -*- lexical-binding: t; -*-

;; ============================================================
;; 编程语言支持
;; ============================================================

(declare-function paredit-mode "paredit")
(defvar paredit-mode-map)
(require 'subtle-delimiter)

(dolist (hook '(scheme-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                racket-mode-hook
                clojure-mode-hook))
  (add-hook hook #'subtle-delimiter-mode))

(defun pandaye/development-eval-expression-paredit-setup ()
  "Enable Paredit in `eval-expression' without stealing RET."
  (require 'paredit)
  (paredit-mode 1)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map paredit-mode-map)
    (define-key map (kbd "RET") #'exit-minibuffer)
    (define-key map (kbd "<return>") #'exit-minibuffer)
    (define-key map "\C-m" #'exit-minibuffer)
    (setq-local minor-mode-overriding-map-alist
                (assq-delete-all 'paredit-mode minor-mode-overriding-map-alist))
    (push `(paredit-mode . ,map) minor-mode-overriding-map-alist)))

(use-package paredit
  :hook
  (racket-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (clojure-mode . paredit-mode)
  (eval-expression-minibuffer-setup . pandaye/development-eval-expression-paredit-setup)
  (ielm-mode . paredit-mode))

(use-package racket-mode
  :mode (("\\.rkt\\'" . racket-mode)
         ("\\.scrbl\\'" . racket-mode))
  :config
  (setq racket-run-in-background t)
  :hook
  (racket-mode . racket-xp-mode))

(use-package beancount
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.bean\\'" . beancount-mode)
  :config
  (define-key beancount-mode-map (kbd "TAB") nil))

(use-package go-mode
  :mode
  ("\\.go\\'" . go-mode))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package clojure-mode)

(use-package cmake-mode
  :mode ("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt))

(use-package corfu-terminal
  :if (< emacs-major-version 31)
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

(defun pandaye/development-common-lisp-completion-setup ()
  "Use Corfu for Common Lisp completion without lsp-bridge conflict."
  (when (and (fboundp 'lsp-bridge-mode)
             (bound-and-true-p lsp-bridge-mode))
    (lsp-bridge-mode -1))
  (when (fboundp 'slime--completion-at-point)
    (remove-hook 'completion-at-point-functions #'slime--completion-at-point t)
    (add-hook 'completion-at-point-functions
              #'pandaye/development-slime-completion-at-point-if-connected nil t))
  (corfu-mode 1))

(defun pandaye/development-slime-completion-at-point-if-connected ()
  "Complete with SLIME when connected or auto-start is enabled."
  (when (and (fboundp 'slime-connected-p)
             (or (slime-connected-p)
                 (not (eq slime-auto-start 'never))))
    (slime--completion-at-point)))

(defalias 'my-slime-completion-at-point-if-connected
  #'pandaye/development-slime-completion-at-point-if-connected)

(add-hook 'lisp-mode-hook #'pandaye/development-common-lisp-completion-setup)
(add-hook 'slime-mode-hook #'pandaye/development-common-lisp-completion-setup)
(add-hook 'slime-repl-mode-hook #'pandaye/development-common-lisp-completion-setup)

(use-package slime
  :commands (slime)
  :init
  (setq inferior-lisp-program "ros run")
  (setq slime-auto-start 'always)
  :mode
  ("\\.ros\\'" . lisp-mode)
  :config
  (slime-setup '(slime-fancy slime-tramp slime-asdf slime-xref-browser))
  (add-hook 'lisp-mode-hook (lambda () (subword-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda () (subword-mode 1))))

;; ============================================================
;; Snippets
;; ============================================================

(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-development)
;;; init-development.el ends here
