;; -*- lexical-binding: t; -*-
;;; my-agent-shell.el --- agent-shell configuration with opencode backend

;; opencode 通过 nvm 安装，Emacs 启动时 exec-path 不含 nvm 路径
;; 在此动态补充，确保 agent-shell 能找到 opencode 可执行文件

(defcustom my/nvm-versions-dir (expand-file-name "~/.nvm/versions/node")
  "NVM node 版本目录路径。"
  :type 'string
  :group 'convenience)

(defun my/add-nvm-node-to-exec-path ()
  "将当前激活的 nvm node bin 目录加入 exec-path 和 PATH。"
  (let* ((nvm-dir my/nvm-versions-dir)
         (node-bins
          (when (file-directory-p nvm-dir)
            (seq-filter #'file-directory-p
                        (mapcar (lambda (v) (expand-file-name "bin" v))
                                (directory-files nvm-dir t "^v" t))))))
    (dolist (bin (nreverse node-bins))  ; 最新版本优先
      (add-to-list 'exec-path bin)
      (setenv "PATH" (concat bin ":" (getenv "PATH"))))))

(my/add-nvm-node-to-exec-path)

;; agent-shell：通过 ACP 协议在 Emacs 内驱动 opencode 等 AI coding agents
(use-package agent-shell
  :config
  ;; 默认使用 opencode 作为 agent
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config))
  :bind
  (("C-c v a" . agent-shell)))

(provide 'my-agent-shell)
;;; my-agent-shell.el ends here
