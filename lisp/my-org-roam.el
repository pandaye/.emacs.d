;; -*- lexical-binding: t; -*-
;;; my-org-roam.el --- Org-roam 双向链接笔记系统配置

;;; Commentary:
;; Org-roam 配置，包括笔记目录、capture 模板、Org-roam UI 和 Super Agenda。

;;; Code:

(condition-case err
    (require 'my-gtd)
  (error (message "my-gtd 加载失败，Org-roam 将无法使用 GTD 基础路径: %s" (error-message-string err))))

;; ============================================================
;; Org-roam 双向链接
;; ============================================================

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (concat org-base-path "/roam")))
  (org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
			 "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)
     ("b" "book" plain (file "~/org/roam/templates/book.txt")
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
			 "#+title: ${title}\n#+roam_tags: book\n\n")
      :unnarrowed t)
     ("p" "project" plain "* GOALS\n\n%?\n\n* TASKS\n\n* NOTES\n"
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
			 "#+title: ${title}\n#+roam_tags: project\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "\n\n* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :empty-lines-after 0
      :unnarrowed t)))
  :bind (;; ("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n o" . org-roam-buffer-toggle)
	 ;; 每日笔记
	 ("C-c n g" . org-roam-dailies-goto-today)
	 ("C-c n y" . org-roam-dailies-goto-yesterday)
	 ("C-c n d" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start nil))

;; ============================================================
;; Agenda 管理 - Super Agenda
;; ============================================================

(use-package org-super-agenda
  :ensure t
  :init
  (org-super-agenda-mode))

(provide 'my-org-roam)
;;; my-org-roam.el ends here