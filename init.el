;;; -*- lexical-binding: t; -*-
;;; pacakge --- Summery
;;; Commentary:
;;; author: pandaye
(require 'package)

;;; Code:
(setq package-enable-at-startup nil)
;; 使用更稳定的镜像源
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)

;; package install
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 use-package
(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; add loadpath
(add-to-list 'load-path
			 (expand-file-name "lisp" user-emacs-directory))
;; add opensource el codes
(add-to-list 'load-path
			 (expand-file-name "opensource" user-emacs-directory))

(require 'my-ui-keyboard)

(load (expand-file-name "pandaye-init.el" user-emacs-directory))
(when (display-graphic-p)
  (load (expand-file-name "gui.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
