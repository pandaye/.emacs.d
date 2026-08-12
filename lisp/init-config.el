;;; init-config.el --- Compose the complete configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the sole composition root.  Keep ordering explicit because startup
;; behavior is part of the configuration contract.

;;; Code:

(require 'init-environment)

(condition-case err
    (require 'init-ui)
  (error (message "my-ui-keyboard 加载失败: %s" (error-message-string err))))

(require 'init-defaults)
(require 'init-local)
(require 'init-editor)
(require 'init-completion)
(require 'init-navigation)
(require 'init-vcs)
(require 'init-org)
(require 'init-markdown)
(require 'init-input-method)
(require 'init-development)
(require 'init-reader)
(require 'init-keybindings)

(provide 'init-config)
;;; init-config.el ends here
