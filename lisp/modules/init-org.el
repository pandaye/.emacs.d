;;; init-org.el --- Org writing and knowledge configuration -*- lexical-binding: t; -*-

;; ============================================================
;; Org 与写作系统
;; ============================================================

(require 'org-ssh)
(require 'tmux-manager)

(use-package htmlize
  :defer t)

(require 'init-org-writing)  ;; Org 外观美化
(require 'static-blog)       ;; Org 静态博客发布
(require 'init-org-gtd)      ;; GTD 任务管理
(require 'org-diary)         ;; 日记系统
(setq daily-diary-base-path (expand-file-name "daily" org-base-path))
(require 'init-org-roam)     ;; Org-roam 双向链接

(unless (featurep 'org-tempo)
  (require 'org-tempo))

(use-package ox-gfm
  :after org)

(provide 'init-org)
;;; init-org.el ends here
