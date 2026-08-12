;;; -*- lexical-binding: t; -*-
;;; pacakge --- Summery
;;; Commentary:
;;; author: pandaye
(require 'package)

;;; Code:
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)
;; 使用更稳定的镜像源
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq package-install-upgrade-built-in t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)

;; 配置 use-package, 某个版本开始 use-package 已经内置在 Emacs 中
(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; add loadpath
(add-to-list 'load-path
			 (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path
			 (expand-file-name "lisp/core" user-emacs-directory))
;; add opensource el codes
(add-to-list 'load-path
			 (expand-file-name "opensource" user-emacs-directory))

(require 'init-config)

(provide 'init)
;;; init.el ends here
