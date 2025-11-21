;; -*- lexical-binding: t; -*-

;; USE lsp-bridge
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lsp-bridge"))

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq acm-enable-copilot t)
(setq tty-child-frames t)
(setq acm-icon-width -1)

;; Configure user langserver directory
(setq lsp-bridge-user-langserver-dir (expand-file-name "~/.emacs.d/lsp-bridge-langserver"))

(setq lsp-bridge-get-project-path-by-filepath
      (lambda (filename)
        "对 Go 文件优先查找 go.mod，其他文件使用默认行为"
        (if (string-match-p "\\.go\\'" filename)
            ;; Go 文件：优先查找 go.mod
            (or (when-let ((go-mod-dir (locate-dominating-file filename "go.mod")))
                  (expand-file-name go-mod-dir))
                ;; 如果没找到 go.mod，回退到 .git
                (when-let ((git-dir (locate-dominating-file filename ".git")))
                  (expand-file-name git-dir)))
          ;; 非 Go 文件，因为 lsp-bridge 用的是 if 所以要写默认行为
          (let* ((result (dir-locals-find-file filename))
                 (dir (if (consp result) (car result) result)))
            (when dir (directory-file-name dir))))))

;; 这里返回 nil 使用默认的配置
(setq lsp-bridge-get-single-lang-server-by-project
      (lambda (project-path filepath)
		;; 存在项目根目录下的 .lsp-bridge.json 则使用该配置文件
        (let ((custom-config (expand-file-name ".lsp-bridge.json" project-path)))
          (when (file-exists-p custom-config)
			custom-config))))

(setq c-basic-offset 4)

(global-set-key (kbd "C-c r d") 'lsp-bridge-find-def)
(global-set-key (kbd "C-c r t") 'lsp-bridge-find-type-def)
(global-set-key (kbd "C-c r r") 'lsp-bridge-find-def-return)
(global-set-key (kbd "C-c r i") 'lsp-bridge-find-impl-other-window)
(global-set-key (kbd "C-c r R") 'lsp-bridge-find-references)
(global-set-key (kbd "C-c r s") 'lsp-bridge-show-documentation)
(global-set-key (kbd "C-c r f") 'lsp-bridge-code-format)
(global-set-key (kbd "C-c r h") 'lsp-bridge-diagnostic-list)

(provide 'my-lsp)
