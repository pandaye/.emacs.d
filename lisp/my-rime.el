;; -*- lexical-binding: t; -*-
;;; my-rime.el --- Rime 中文输入法配置

;;; Commentary:
;; Rime 输入法配置，包括输入方案切换。
;; 光标颜色联动由 my-cursor.el 统一管理。

;;; Code:

(defun my/set-rime-jp ()
  "切换到日语输入方案。"
  (interactive)
  (rime-lib-select-schema "jaroomaji"))

(defun my/set-rime-zh ()
  "切换到中文输入方案。"
  (interactive)
  (rime-lib-select-schema "tigress"))

(use-package rime
  :init
  (let ((librime-path (expand-file-name "~/.emacs.d/librime")))
    (when (file-directory-p librime-path)
      (setq rime-librime-root librime-path)))
  :config
  ;; 在非插入模式、字母后、代码中禁用输入法
  (setq rime-disable-predicates
        '(meow-not-insert-p
          rime-predicate-after-alphabet-char-p
	  rime-predicate-space-after-cc-p
          rime-predicate-prog-in-code-p))
  (add-hook 'kill-emacs-hook #'rime-lib-finalize)
  :bind
  (("C-c i j" . my/set-rime-jp)
   ("C-c i f" . my/set-rime-zh))
  :custom
  (default-input-method "rime"))

;; 输入法切换快捷键
(global-set-key (kbd "C-c i i") 'toggle-input-method)

(provide 'my-rime)
;;; my-rime.el ends here
