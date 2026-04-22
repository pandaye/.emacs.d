;; -*- lexical-binding: t; -*-
;;; my-gtd.el --- GTD 和 Org Agenda 配置

;;; Commentary:
;; GTD（Getting Things Done）任务管理系统，包括 org-todo 关键字、
;; capture 模板、agenda 视图和 refile 设置。

;;; Code:

;; ============================================================
;; 基础路径配置
;; ============================================================

(defcustom org-base-path (expand-file-name "~/.pandaye-journal")
  "Org 文件的基础路径，用于 GTD、日记和 Org-roam。"
  :type 'string
  :group 'org)

(defvar org-gtd-file
  (concat org-base-path "/project.org")
  "GTD 主文件路径。")

(defvar my/issue-file (expand-file-name "issue.org" org-base-path)
  "Issue inbox 文件路径。")

;; ============================================================
;; TODO 关键字与样式
;; ============================================================

(setq org-todo-keywords
      '((sequence "TODO(t!)" "PROCESSING(p!)" "REVIEWING(r!)" "BLOCK(b!)" "LATER(l!)" "|" "DONE(d!)" "CANCEL(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#66cccc"    :weight bold))
        ("BLOCK" . (:foreground "red"    :weight bold))
        ("LATER" . (:foreground "yellow"    :weight bold))
        ("PROCESSING" . (:foreground "orange" :weight bold))
        ("REVIEWING" . (:foreground "#a5d6ff" :weight bold))
        ("DONE" . (:foreground "green"  :weight bold))
        ("CANCEL" . (:foreground "grey"  :weight bold))))

;; ============================================================
;; 优先级配置
;; ============================================================

;; 优先级标记颜色（#A #B #C，A 最高）
(setq org-priority-faces
      '((?A . (:foreground "#ff5555" :background "#3c1515" :weight bold :box t))
        (?B . (:foreground "#ffb86c" :weight bold))
        (?C . (:foreground "#6272a4"))))

;; 调整优先级范围（默认 A/B/C，可以不改）
(setq org-priority-highest ?A)
(setq org-priority-lowest ?C)
(setq org-priority-default ?B)

(setq org-log-into-drawer t)

;; ============================================================
;; Capture 模板
;; ============================================================

(setq org-capture-templates
      `(
        ;; 日常任务：直接写入 issue.org 顶层（inbox 文件）
        ("i" "Issue (inbox todo)" entry
         (file ,my/issue-file)
         "* TODO %?\n  %U\n  %a\n"
         :empty-lines 1)
        ))

(defun gtd ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))

;; ============================================================
;; Agenda 配置
;; ============================================================

(setq org-agenda-files (list org-base-path))

(setq org-agenda-skip-timestamp-if-done t)
;; 允许 refile 到 agenda 文件
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
;; 显示完整路径（包括文件名）
(setq org-refile-use-outline-path 'file)
;; 允许按完整路径补全
(setq org-outline-path-complete-in-steps nil)

;; 自定义 Agenda 命令 - Weekly Review
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
               '("w" "Weekly Review"
                 ((agenda "" ((org-agenda-span 'week)
                              (org-agenda-start-on-weekday 1)))
                  (todo "REVIEWING"
                        ((org-agenda-overriding-header "Tasks in Review")))
                  (todo "PROCESSING"
                        ((org-agenda-overriding-header "In Progress")))
                  (todo "TODO"
                        ((org-agenda-overriding-header "Todo Items")))
                  (todo "BLOCK"
                        ((org-agenda-overriding-header "Blocked Tasks")))
                  (todo "LATER"
                        ((org-agenda-overriding-header "Scheduled for Later")))))))

;; ============================================================
;; 快捷键
;; ============================================================

(global-set-key (kbd "C-c o g") 'gtd)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l c") 'org-capture)
(global-set-key (kbd "C-c l l") 'org-store-link)
(global-set-key (kbd "C-c l r") 'org-clock-report)

;; ============================================================
;; Agenda 着色
;; ============================================================

(defun my/org-agenda-colorize-category ()
  "Colorize only the category part of agenda items based on source file."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((cat (get-text-property (point) 'org-category))
             (color (when cat
                      (cl-loop for (pattern . col) in my/org-agenda-file-colors
                                when (string-match-p pattern cat)
                                return col))))
        (when color
          (let ((inhibit-read-only t)
                (line-end (line-end-position)))
            (when (re-search-forward (concat "^[[:space:]]*\\(" (regexp-quote cat) "\\):?") line-end t)
              (add-face-text-property (match-beginning 1) (match-end 1)
                                      `(:foreground ,color))))))
      (forward-line 1))))

(defvar my/org-agenda-file-colors
  '(("project" . "#ff7b72")
    ("issue" . "#79c0ff")
    ("daily" . "#a5d6ff"))
  "文件名到颜色的映射，用于 agenda 条目分类着色。")

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-colorize-category)

(provide 'my-gtd)
;;; my-gtd.el ends here
