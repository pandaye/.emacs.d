;; -*- lexical-binding: t; -*-
;;; my-lsp.el --- LSP Bridge 配置（延迟加载）

;;; Commentary:
;; LSP 配置延迟加载，仅在进入编程模式时激活。
;; 通过 pandaye-init.el 中的 hook 触发加载。
;;
;; lsp-bridge 不支持标准 package-vc（无 autoload cookies），
;; 使用 package-vc-install 拉取源码 + 手动 load-path 方式管理。

;;; Code:

;; 确保 lsp-bridge 仓库已克隆到 elpa 目录
(let ((lsp-bridge-dir (expand-file-name "elpa/lsp-bridge" user-emacs-directory)))
  (unless (file-exists-p (expand-file-name "lsp-bridge.el" lsp-bridge-dir))
    (package-vc-install '(lsp-bridge :url "https://github.com/manateelazycat/lsp-bridge")))
  (add-to-list 'load-path lsp-bridge-dir))

(use-package lsp-bridge
  :ensure nil
  :demand t
  :custom
  (acm-enable-copilot t)
  (tty-child-frames t)
  (acm-icon-width -1)
  (c-basic-offset 4)
  (lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge-langserver" user-emacs-directory))
  :config
  (setq lsp-bridge-get-project-path-by-filepath
        (lambda (filename)
          "对 Go 文件优先查找 go.mod，其他文件使用默认行为"
          (if (string-match-p "\\.go\\'" filename)
              (or (when-let* ((go-mod-dir (locate-dominating-file filename "go.mod")))
                    (expand-file-name go-mod-dir))
                (when-let* ((git-dir (locate-dominating-file filename ".git")))
                  (expand-file-name git-dir)))
            (let* ((result (dir-locals-find-file filename))
                   (dir (if (consp result) (car result) result)))
              (when dir (directory-file-name dir))))))

  (setq lsp-bridge-get-single-lang-server-by-project
        (lambda (project-path filepath)
          ;; 存在项目根目录下的 .lsp-bridge.json 则使用该配置文件
          (let ((custom-config (expand-file-name ".lsp-bridge.json" project-path)))
            (when (file-exists-p custom-config)
              custom-config))))

  (global-lsp-bridge-mode)

  :bind
  (("C-c r d" . lsp-bridge-find-def)
   ("C-c r t" . lsp-bridge-find-type-def)
   ("C-c r r" . lsp-bridge-find-def-return)
   ("C-c r i" . lsp-bridge-find-impl-other-window)
   ("C-c r R" . lsp-bridge-find-references)
   ("C-c r s" . lsp-bridge-show-documentation)
   ("C-c r f" . lsp-bridge-code-format)
   ("C-c r h" . lsp-bridge-diagnostic-list)))

(provide 'my-lsp)
;;; my-lsp.el ends here
