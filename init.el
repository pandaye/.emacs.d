;;; init.el --- Entry point -*- lexical-binding: t; -*-
;;; Commentary:
;; 入口只做三件事：初始化包管理、加载用户的 custom-file、把配置根目录
;; `lisp/' 挂到 `load-path' 后交给 `init-config'。
;;
;; 三个层目录（core/、modules/、features/）由 `init-environment' 统一
;; 登记，这里不重复声明。

;;; Code:

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

;; 只登记配置根目录：`init-config'（组合根）和 `init-environment'（层目录
;; 登记）都在这一层，能 `require' 到它们即可。
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-config)

(provide 'init)
;;; init.el ends here
