;; -*- lexical-binding: t; -*-
;;; my-org-roam.el --- Org-roam 双向链接笔记系统配置

;;; Commentary:
;; Org-roam 配置，包括笔记目录、capture 模板、Org-roam UI 和 Super Agenda。

;;; Code:

(condition-case err
    (require 'init-org-gtd)
  (error (message "my-gtd 加载失败，Org-roam 将无法使用 GTD 基础路径: %s" (error-message-string err))))

(require 'org-listing)

(defun my/org-roam--file-level-nodes ()
  "Return unique top-level Org-roam nodes, one per file."
  (require 'org-roam)
  (let ((seen-files (make-hash-table :test 'equal))
        nodes)
    (dolist (node (org-roam-node-list))
      (let ((file (org-roam-node-file node))
            (level (or (org-roam-node-level node) 0)))
        (when (and (zerop level)
                   (not (gethash file seen-files)))
          (puthash file t seen-files)
          (push node nodes))))
    (nreverse nodes)))

(defun my/org-roam--list-items ()
  "Return Org-roam note listing items sorted by file mtime descending."
  (mapcar (lambda (node)
            (let* ((mtime (org-roam-node-file-mtime node))
                   (file (org-roam-node-file node))
                   (title (or (org-roam-node-title node)
                              (file-name-base file))))
              (list :group (my/org-list-group-label mtime)
                    :file file
                    :title title
                    :mtime mtime)))
          (seq-sort (lambda (a b)
                      (time-less-p (org-roam-node-file-mtime b)
                                   (org-roam-node-file-mtime a)))
                    (my/org-roam--file-level-nodes))))

(defun my/org-roam--insert-list-item (item)
  "Insert one Org-roam ITEM into the current listing buffer."
  (insert (format "- [%s] %s\n"
                  (format-time-string "%Y-%m-%d %H:%M" (plist-get item :mtime))
                  (my/org-list-make-link (plist-get item :file)
                                         (plist-get item :title)))))

(defun my/org-roam-list-notes-by-mtime ()
  "Show Org-roam notes grouped by year-month and paged by file mtime."
  (interactive)
  (my/org-list-open-buffer "*Org Roam Notes*"
                           "Org-roam Notes"
                           #'my/org-roam--list-items
                           #'my/org-roam--insert-list-item))

;; ============================================================
;; Org-roam 双向链接
;; ============================================================

(use-package org-roam
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture
             org-roam-buffer-toggle)
  :custom
  (org-roam-directory (file-truename (concat org-base-path "/roam")))
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%(format-time-string \"%Y%m%d%H%M%S\")-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)))
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
  :after org-agenda
  :config
  (org-super-agenda-mode))

(provide 'init-org-roam)
;;; my-org-roam.el ends here
