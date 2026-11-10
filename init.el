;;; init.el --- 配置入口 -*- lexical-binding: t; -*-
;;; Commentary:
;; 启动分三段，顺序即契约：
;;   1. 包管理：镜像、`package-initialize'、use-package；
;;   2. 加载用户的 custom-file；
;;   3. 登记 core/modules/features 三个层目录，再按依赖方向加载模块。
;;
;; 依赖方向：core/modules -> features -> Emacs 内置库。

;;; Code:

;; ── 包管理 ────────────────────────────────────────────────────────────

(require 'package)

(setq load-prefer-newer t
      package-enable-at-startup nil
      ;; 使用更稳定的镜像源
      package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-install-upgrade-built-in t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)

;; use-package 自 Emacs 29 起内置
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ── 层目录 ────────────────────────────────────────────────────────────
;; 每个层目录只登记一次。`add-to-list' 把新条目放在最前，所以最终优先级
;; 是 features > modules > core。

(dolist (layer '("core" "modules" "features"))
  (add-to-list 'load-path
               (expand-file-name layer
                                 (expand-file-name "lisp" user-emacs-directory))))

;; ── 模块加载顺序 ──────────────────────────────────────────────────────

;; 界面与模态编辑先行：后续模块依赖它建立的 face、meow 状态和键位基础。
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

(provide 'init)
;;; init.el ends here
