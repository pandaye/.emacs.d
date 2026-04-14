;; -*- lexical-binding: t; -*-
;;; my-lsp.el --- LSP Bridge 配置（延迟加载）

;;; Commentary:
;; LSP 配置延迟加载，仅在进入编程模式时激活。
;; 通过 pandaye-init.el 中的 hook 触发加载。

;;; Code:

;; USE lsp-bridge
(add-to-list 'load-path (expand-file-name "lsp-bridge" user-emacs-directory))

(condition-case err
    (progn
      (require 'lsp-bridge)
      (global-lsp-bridge-mode))
  (error (message "lsp-bridge 加载失败: %s" (error-message-string err))))

(setq acm-enable-copilot t)
(setq tty-child-frames t)
(setq acm-icon-width -1)

(setq c-basic-offset 4)

;; Configure user langserver directory
(setq lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge-langserver" user-emacs-directory))

(setq lsp-bridge-get-project-path-by-filepath
      (lambda (filename)
        "对 Go 文件优先查找 go.mod，其他文件使用默认行为"
        (if (string-match-p "\\.go\\'" filename)
            (or (when-let ((go-mod-dir (locate-dominating-file filename "go.mod")))
                  (expand-file-name go-mod-dir))
              (when-let ((git-dir (locate-dominating-file filename ".git")))
                (expand-file-name git-dir)))
          (let* ((result (dir-locals-find-file filename))
                 (dir (if (consp result) (car result) result)))
            (when dir (directory-file-name dir))))))

;; 返回 nil 使用默认的配置
(setq lsp-bridge-get-single-lang-server-by-project
      (lambda (project-path filepath)
	;; 存在项目根目录下的 .lsp-bridge.json 则使用该配置文件
        (let ((custom-config (expand-file-name ".lsp-bridge.json" project-path)))
          (when (file-exists-p custom-config)
	    custom-config))))

(global-set-key (kbd "C-c r d") 'lsp-bridge-find-def)
(global-set-key (kbd "C-c r t") 'lsp-bridge-find-type-def)
(global-set-key (kbd "C-c r r") 'lsp-bridge-find-def-return)
(global-set-key (kbd "C-c r i") 'lsp-bridge-find-impl-other-window)
(global-set-key (kbd "C-c r R") 'lsp-bridge-find-references)
(global-set-key (kbd "C-c r s") 'lsp-bridge-show-documentation)
(global-set-key (kbd "C-c r f") 'lsp-bridge-code-format)
(global-set-key (kbd "C-c r h") 'lsp-bridge-diagnostic-list)

(provide 'my-lsp)
;;; my-lsp.el ends here