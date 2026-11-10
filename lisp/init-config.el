;;; init-config.el --- Compose the complete configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the sole composition root.  Keep ordering explicit because startup
;; behavior is part of the configuration contract.
;;
;; `init-environment' 登记三个层目录（core/modules/features），必须先于
;; 任何层内模块加载。

;;; Code:

(require 'init-environment)

(condition-case err
    (require 'init-ui)
  (error (message "界面与模态编辑配置加载失败: %s"
                  (error-message-string err))))

;; 显示环境分流：`init-tui' 只服务终端帧，`init-gui' 只服务图形帧。
;; 两者都在加载时判断一次，并对之后出现的帧挂同名 hook，所以
;; `emacs --daemon' + emacsclient（图形或 -nw）都能拿到对应配置。
(require 'init-tui)
(require 'init-gui)

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
(require 'init-dsh)
(require 'init-keybindings)

(provide 'init-config)
;;; init-config.el ends here
