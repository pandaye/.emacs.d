;; -*- lexical-binding: t; -*-
;;; my-rime.el --- Rime 中文输入法与光标颜色配置

;;; Commentary:
;; Rime 输入法配置，包括输入方案切换、Meow 状态联动光标颜色。
;; 依赖 my-cursor.el 提供光标颜色管理函数。

;;; Code:

(condition-case err
    (require 'my-cursor)
  (error (message "my-cursor 加载失败: %s" (error-message-string err))))

(defun my/set-rime-jp ()
  "切换到日语输入方案。"
  (interactive)
  (rime-lib-select-schema "jaroomaji"))

(defun my/set-rime-zh ()
  "切换到中文输入方案。"
  (interactive)
  (rime-lib-select-schema "tigress"))

(use-package rime
  :ensure t
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
  ;; Rime 模式变化时更新光标颜色
  (add-hook 'rime-mode-hook #'my/update-cursor-by-rime-state)
  (add-hook 'kill-emacs-hook #'rime-lib-finalize)
  :bind
  (("C-c i j" . my/set-rime-jp)
   ("C-c i f" . my/set-rime-zh))
  :custom
  (default-input-method "rime"))

;; 监听窗口和 buffer 变化
(add-hook 'window-buffer-change-functions #'my/update-cursor-on-frame-change)
(add-hook 'window-selection-change-functions #'my/update-cursor-on-frame-change)

;; 输入法切换快捷键
(global-set-key (kbd "C-c i i") 'toggle-input-method)

(provide 'my-rime)
;;; my-rime.el ends here