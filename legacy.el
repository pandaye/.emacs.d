(setq column-number-mode t)
(global-hl-line-mode t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; 设置默认 Tab 宽度
(setq-default tab-width 4)

(set-face-attribute 'fringe nil :background nil)

(define-fringe-bitmap 'left-arrow [])
(define-fringe-bitmap 'left-curly-arrow [])
(define-fringe-bitmap 'left-triangle [])

(defface fallback '((t :family "Fira Code Light"
                       :foreground "gray")) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
                        
(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 3)
  (smooth-scrolling-mode 1))

(defalias 'list-buffers 'ibuffer)

(winner-mode 1)
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-toggle)
  (global-set-key [f7] 'neotree-find)
  (setq neo-theme 'arrow))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(require 'myscheme)
(use-package racket-mode
  :ensure t
  :config
  (setq racket-racket-program "racket")
  (setq racket-raco-program "raco")
  :bind
  (:map racket-mode-map
        ("C-x C-j" . racket-run)))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . enable-paredit-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; 配置输出指令
  (setq markdown-command
        "pandoc -f markdown -t html -s -c ~/.emacs.d/markdown/style.css --mathjax --highlight-style pygments"))

(use-package ox-gfm
  :ensure ox-gfm)

(use-package yaml-mode
  :ensure t)

(use-package cmake-mode
  :hook
  ((cmake-mode . lsp)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map [C-tab] 'yas-expand))
(use-package yasnippet-snippets
  :ensure t)

(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  ;; Highlight changes on editing.
  (diff-hl-flydiff-mode)
  ;; Makes fringe and margin react to mouse clicks to show the curresponding hunk.
  (diff-hl-show-hunk-mouse-mode)
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background "#e9cd43"))))
  (diff-hl-insert ((t (:background "#03e94f"))))
  (diff-hl-delete ((t (:background "#f5597e")))))

(global-set-key (kbd "<f9>") 'eshell)
