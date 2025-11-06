;; USE LSP-MODE with company
;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode 1)
;;   :config
;;   ;; 可选：补全菜单延迟、最小输入字符数等
;;   (setq company-idle-delay 0.2
;;         company-minimum-prefix-length 2
;;         company-selection-wrap-around t
;;         company-tooltip-align-annotations t
;; 		company-backends '((company-capf company-files))))
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (beancount-mode . lsp-deferred)
;;   :config
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "beancount-language-server")
;;     :major-modes '(beancount-mode)
;;     :server-id 'beancount-language-server
;;     :priority 10
;;     :initialization-options
;;     (lambda () (list :journal_file (concat (projectile-project-root) "main.bean")
;;                      :formatting (list
;; 								  :prefix_width 30
;; 								  :currency_column 60
;; 								  :number_currency_spacing 1
;; 								  :account_amount_spacing 2)))))
;;   :commands (lsp lsp-deferred))

;; USE lsp-bridge
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lsp-bridge"))

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(unless (display-graphic-p)
  (with-eval-after-load 'acm
    (require 'acm-terminal)))

(provide 'my-lsp)
