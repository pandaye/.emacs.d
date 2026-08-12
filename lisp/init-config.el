;;; init-config.el --- Compose the complete configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the sole composition root.  Keep ordering explicit because startup
;; behavior is part of the configuration contract.

;;; Code:

(require 'init-environment)

(condition-case err
    (require 'my-ui-keyboard)
  (error (message "my-ui-keyboard 加载失败: %s" (error-message-string err))))

(require 'init-defaults)
(require 'init-local)
(require 'my-editor)
(require 'my-navigation)
(require 'my-vcs)
(require 'my-writing)
(require 'my-development)
(require 'my-reader)
(require 'my-keybindings)
(require 'init-keybindings)

(provide 'init-config)
;;; init-config.el ends here
