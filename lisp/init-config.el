;;; init-config.el --- Compose the complete configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the sole composition root.  Keep ordering explicit because startup
;; behavior is part of the configuration contract.

;;; Code:

(require 'init-environment)

(condition-case err
    (require 'init-ui)
  (error (message "界面与模态编辑配置加载失败: %s"
                  (error-message-string err))))

(require 'init-defaults)
(require 'init-local)
(require 'init-editor)
(require 'init-completion)
(require 'init-navigation)
(require 'init-vcs)
(require 'init-org)
(require 'init-org-writing)
(require 'init-org-gtd)
(require 'init-org-roam)
(require 'init-markdown)
(require 'init-input-method)
(require 'init-development)
(require 'init-lsp)
(require 'init-reader)
(require 'init-keybindings)

(provide 'init-config)
;;; init-config.el ends here
