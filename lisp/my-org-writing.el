(use-package rime
  :ensure t
  :init
  (setq rime-librime-root (expand-file-name "~/.emacs.d/librime"))
  :config
  (setq rime-disable-predicates
	  '(meow-not-insert-p
	    rime-predicate-after-alphabet-char-p
            rime-predicate-prog-in-code-p))
  :custom
  (default-input-method "rime"))

;; --------
;; GTD 配置
;; --------
(setq org-todo-keywords
      '((sequence "TODO(p!)" "PROCESSING(t!)" "BLOCK(s!)" "|" "DONE(d!)" "CANCEL(a@/!)")))

;; 设置任务样式
(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#66cccc"    :weight bold))
        ("BLOCK" . (:foreground "red"    :weight bold))
        ("PROCESSING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green"  :weight bold))
        ("CANCEL" . (:foreground "black"  :weight bold))
        ))

(setq org-base-path (expand-file-name "~/.pandaye-journal"))
(defvar org-gtd-file
  (concat org-base-path "/project.org"))

(defun gtd ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))

;; 加入到日程列表里 - 设置整个目录，自动包含所有 .org 文件
(setq org-agenda-files (list org-base-path))

;; 快捷键设置
;; 设置 Org Agenda 快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
;; 将 GTD 快捷键改为 C-c o g，避免与 counsel-git 冲突
(global-set-key (kbd "C-c o g") 'gtd)

;; 其他未归档的配置
;;
(font-lock-add-keywords
 'org-mode
 '(("^ *- State \\(\"[A-Z]+\"\\).*\\[.*\\]"
    0 'font-lock-comment-face t)))

;; -----------------
;; org-roam 双向链接
;; -----------------
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename org-base-path))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; ------------
;; 日程管理优化
;; ------------
(use-package org-super-agenda
  :ensure t
  :init
  (org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:auto-parent t))))  ;; 自动按父 headline 分组


(provide 'my-org-writing)
