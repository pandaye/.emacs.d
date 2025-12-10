;; 基础增强
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(pixel-scroll-precision-mode 1)

(use-package powerline
  :config
  (powerline-default-theme))

;; ;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "Sarasa Mono SC")))

;; (setq face-font-rescale-alist '(("Sarasa Mono SC" . 1.2)))

(set-frame-font "JetBrains Maple Mono-13" nil t)

(use-package org-modern
  :ensure t
  :config
  (global-org-modern-mode))

;; EAF not support native wayland
;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (require 'eaf-browser)
;;   (require 'eaf-pdf-viewer)
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

;; Linux GUI 剪贴板配置
(defun linux-gui-clipboard-setup ()
  "Setup clipboard integration for GUI Emacs on Linux."
  (when (and (eq system-type 'gnu/linux)
			 (display-graphic-p))
    ;; 启用与系统剪贴板的交互
	(setq select-enable-clipboard t
		  select-enable-primary t
		  save-interprogram-paste-before-kill t
		  ;; 使用 X clipboard
		  x-select-enable-clipboard t
		  x-select-enable-primary t
		  ;; 鼠标中键粘贴主选择区
		  mouse-drag-copy-region t)))

(linux-gui-clipboard-setup)

(provide 'gui)
