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
      '((sequence "TODO(t!)" "PROCESSING(p!)" "BLOCK(b!)" "LATER(l!)" "|" "REVIEWING(r!)" "DONE(d!)" "CANCEL(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO"       . (:foreground "#ff5f5f" :weight bold))   ; 更鲜艳的亮红
        ("PROCESSING" . (:foreground "#ff9f1c" :weight bold))   ; 更鲜艳的亮橙
        ("BLOCK"      . (:foreground "#ff6b2c" :weight bold))   ; 更醒目的阻塞态
        ("LATER"      . (:foreground "#4db6ff" :weight bold))   ; 更鲜艳的冷蓝
        ("REVIEWING"  . (:foreground "#7c6f64" :weight bold))   ; 完成态压暗
        ("DONE"       . (:foreground "#5a8f63" :weight bold))   ; 完成态压暗
        ("CANCEL"     . (:foreground "#6c6f73" :weight bold)))) ; 完成态压暗

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

;; LOGBOOK / PROPERTIES drawer 颜色淡化，避免干扰 TODO 关键字
(set-face-attribute 'org-drawer nil :foreground "#665c54")
(set-face-attribute 'org-special-keyword nil :foreground "#7c6f64")
;; 时间戳单独降饱和，避免和一级标题抢视觉重心。
(set-face-attribute 'org-date nil :foreground "#6f8f8f")
;; Headline 加粗，增强 GTD 文件层级可读性。
(dolist (face '(org-level-1
                org-level-2
                org-level-3
                org-level-4
                org-level-5
                org-level-6
                org-level-7
                org-level-8))
  (set-face-attribute face nil :weight 'bold))

(setq org-log-done 'time)
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
(defun my/org-last-week-range ()
  "Return the start and end time of last week as a cons cell.
Weeks start on Monday, matching the agenda configuration."
  (let* ((now (decode-time (current-time)))
         (day-of-week (decoded-time-weekday now))
         (days-since-monday (mod (- day-of-week 1) 7))
         (today-midnight (encode-time 0 0 0
                                      (decoded-time-day now)
                                      (decoded-time-month now)
                                      (decoded-time-year now)))
         (this-monday (time-subtract today-midnight
                                     (days-to-time days-since-monday)))
         (last-monday (time-subtract this-monday (days-to-time 7))))
    (cons last-monday this-monday)))

(defun my/org-agenda-skip-not-completed-last-week ()
  "Skip entries not completed during last week."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (completed-time (my/org-entry-completed-time subtree-end))
         (range (my/org-last-week-range))
         (start (car range))
         (end (cdr range)))
    (if (and completed-time
             (not (time-less-p completed-time start))
             (time-less-p completed-time end))
        nil
      subtree-end)))

(defun my/org-entry-completed-time (subtree-end)
  "Return completion time for current entry before SUBTREE-END.
Prefer CLOSED, then the latest done-state transition in LOGBOOK,
then ARCHIVE_TIME for archived entries."
  (or (let ((closed (org-entry-get (point) "CLOSED")))
        (and closed (org-time-string-to-time closed)))
      (save-excursion
        (let ((case-fold-search nil)
              (latest nil)
              (done-regexp (regexp-opt org-done-keywords)))
          (while (re-search-forward
                  (format "^[ \t]*- State \"%s\".*\\(\\[[^]]+\\]\\)" done-regexp)
                  subtree-end t)
            (setq latest (org-time-string-to-time (match-string 1))))
          latest))
      (let ((archive-time (org-entry-get (point) "ARCHIVE_TIME")))
        (and archive-time
             (org-time-string-to-time
              (concat "[" archive-time "]"))))))

(defun my/org-agenda-files-with-archives ()
  "Return agenda files plus archived Org files under `org-base-path'."
  (delete-dups
   (append (org-agenda-files t)
           (directory-files-recursively org-base-path "\\.org_archive\\'"))))

(defun my/org-done-keywords ()
  "Return configured done keywords reliably."
  (or org-done-keywords
      (let (done-seen done-keywords)
        (dolist (sequence org-todo-keywords)
          (when (eq (car sequence) 'sequence)
            (dolist (keyword (cdr sequence))
              (cond
               ((string= keyword "|")
                (setq done-seen t))
               (done-seen
                (push (car (split-string keyword "[({]" t)) done-keywords))))))
        (nreverse done-keywords))))

(defun my/org-last-week-completed-blocks ()
  "Return agenda blocks for last week's completed items, grouped by state."
  (mapcar (lambda (keyword)
            `(todo ,keyword
                   ((org-agenda-overriding-header ,(format "%s Last Week" keyword))
                    (org-agenda-files (my/org-agenda-files-with-archives))
                    (org-agenda-skip-function #'my/org-agenda-skip-not-completed-last-week)
                    (org-agenda-prefix-format '((todo . " %(my/org-agenda-completed-time-prefix) %-12:c")))
                    (org-agenda-sorting-strategy '(time-down priority-down category-keep)))))
          (my/org-done-keywords)))

(defun my/org-agenda-completed-time-prefix ()
  "Return a formatted completion timestamp for agenda prefixes."
  (let ((completed-time (my/org-entry-completed-time
                         (save-excursion (org-end-of-subtree t)))))
    (if completed-time
        (format-time-string "%m-%d %a %H:%M " completed-time)
      "")))

(with-eval-after-load 'org-agenda
  ;; Use a quieter separator in multi-block agenda views.
  (setq org-agenda-block-separator ?─)
  (set-face-attribute 'org-time-grid nil :foreground "#665c54")
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
                        ((org-agenda-overriding-header "Scheduled for Later"))))))
  (add-to-list 'org-agenda-custom-commands
               `("W" "Last Week Completed"
                 ,(my/org-last-week-completed-blocks))))

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

(defun my/org-agenda-dim-block-separators ()
  "Tone down block separators in agenda buffers."
  (let ((separator org-agenda-block-separator))
    (when separator
      (save-excursion
        (goto-char (point-min))
        (let ((regexp (if (stringp separator)
                          (format "^%s$" (regexp-quote separator))
                        (format "^%c+$" separator))))
          (while (re-search-forward regexp nil t)
            (add-face-text-property
             (match-beginning 0) (match-end 0)
             '(:foreground "#665c54"))))))))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-colorize-category)
(add-hook 'org-agenda-finalize-hook #'my/org-agenda-dim-block-separators)

(provide 'my-gtd)
;;; my-gtd.el ends here
