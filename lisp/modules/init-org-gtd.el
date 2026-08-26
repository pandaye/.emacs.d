;; -*- lexical-binding: t; -*-
;;; init-org-gtd.el --- GTD 和 Org Agenda 配置

;;; Commentary:
;; GTD（Getting Things Done）任务管理系统，包括 org-todo 关键字、
;; capture 模板、agenda 视图和 refile 设置。

;;; Code:

;; ============================================================
;; 基础路径配置
;; ============================================================

(defcustom org-base-path (expand-file-name "~/.org-journal")
  "Org 文件的基础路径，用于 GTD、日记和 Org-roam。
请在 local-vars.local.el 中覆盖此值。"
  :type 'string
  :group 'org)

(defvar org-gtd-file
  (concat org-base-path "/project.org")
  "GTD 主文件路径。")

(defvaralias 'my/issue-file 'pandaye/org-issue-file)

(defvar pandaye/org-issue-file (expand-file-name "issue.org" org-base-path)
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
         (file ,pandaye/org-issue-file)
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
(defun pandaye/org-last-week-range ()
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

(defun pandaye/org-this-week-range ()
  "Return the start and end time of this week as a cons cell.
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
         (next-monday (time-add this-monday (days-to-time 7))))
    (cons this-monday next-monday)))

(defun pandaye/org-agenda-skip-not-updated-last-week ()
  "Skip entries whose latest state change did not happen during last week."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (state-change-time (pandaye/org-entry-last-state-change-time subtree-end))
         (range (pandaye/org-last-week-range))
         (start (car range))
         (end (cdr range)))
    (if (and state-change-time
             (not (time-less-p state-change-time start))
             (time-less-p state-change-time end))
        nil
      subtree-end)))

(defun pandaye/org-entry-last-state-change-time (subtree-end)
  "Return latest state change time for current entry before SUBTREE-END.
Prefer the latest TODO state transition in LOGBOOK, then CLOSED,
then ARCHIVE_TIME for archived entries."
  (or (save-excursion
        (let ((case-fold-search nil)
              (latest nil)
              (todo-regexp (regexp-opt (pandaye/org-all-todo-keywords))))
          (while (re-search-forward
                  (format "^[ \t]*- State \"%s\".*\\(\\[[^]]+\\]\\)" todo-regexp)
                  subtree-end t)
            (setq latest (org-time-string-to-time (match-string 1))))
          latest))
      (let ((closed (org-entry-get (point) "CLOSED")))
        (and closed (org-time-string-to-time closed)))
      (let ((archive-time (org-entry-get (point) "ARCHIVE_TIME")))
        (and archive-time
             (org-time-string-to-time
              (concat "[" archive-time "]"))))))

(defun pandaye/org-entry-closed-time (subtree-end)
  "Return closing time for current entry before SUBTREE-END.
Prefer CLOSED, then the latest done-state transition in LOGBOOK,
then ARCHIVE_TIME for archived entries."
  (or (let ((closed (org-entry-get (point) "CLOSED")))
        (and closed (org-time-string-to-time closed)))
      (save-excursion
        (let ((case-fold-search nil)
              (latest nil)
              (done-regexp (regexp-opt (pandaye/org-done-keywords))))
          (while (re-search-forward
                  (format "^[ \t]*- State \"%s\".*\\(\\[[^]]+\\]\\)" done-regexp)
                  subtree-end t)
            (setq latest (org-time-string-to-time (match-string 1))))
          latest))
      (let ((archive-time (org-entry-get (point) "ARCHIVE_TIME")))
        (and archive-time
             (org-time-string-to-time
              (concat "[" archive-time "]"))))))

(defun pandaye/org-agenda-files-with-archives ()
  "Return agenda files plus archived Org files under `org-base-path'."
  (delete-dups
   (append (org-agenda-files t)
           (directory-files-recursively org-base-path "\\.org_archive\\'"))))

(defun pandaye/org-done-keywords ()
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

(defun pandaye/org-closed-keywords ()
  "Return keywords treated as truly closed items."
  (seq-filter (lambda (keyword)
                (member keyword '("DONE" "CANCEL")))
              (pandaye/org-done-keywords)))

(defun pandaye/org-all-todo-keywords ()
  "Return configured TODO keywords reliably."
  (or org-todo-keywords-1
      (let (todo-keywords)
        (dolist (sequence org-todo-keywords)
          (when (eq (car sequence) 'sequence)
            (dolist (keyword (cdr sequence))
              (unless (string= keyword "|")
                (push (car (split-string keyword "[({]" t)) todo-keywords)))))
        (nreverse todo-keywords))))

(defun pandaye/org-agenda-skip-planned-outside-week ()
  "Skip entries with SCHEDULED or DEADLINE outside the current week.
Entries without any planning timestamp are kept."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (range (pandaye/org-this-week-range))
         (start (car range))
         (end (cdr range))
         (planning (or (org-entry-get (point) "SCHEDULED")
                       (org-entry-get (point) "DEADLINE"))))
    (if (and planning
             (let ((time (org-time-string-to-time planning)))
               (or (time-less-p time start)
                   (not (time-less-p time end)))))
        subtree-end
      nil)))

(defun pandaye/org-agenda-skip-not-closed-this-week ()
  "Skip entries not closed during this week."
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (closed-time (pandaye/org-entry-closed-time subtree-end))
         (range (pandaye/org-this-week-range))
         (start (car range))
         (end (cdr range)))
    (if (and closed-time
             (not (time-less-p closed-time start))
             (time-less-p closed-time end))
        nil
      subtree-end)))

(defun pandaye/org-last-week-status-update-blocks ()
  "Return agenda blocks for items updated last week, grouped by current state."
  (mapcar (lambda (keyword)
            `(todo ,keyword
                   ((org-agenda-overriding-header ,(format "%s Updated Last Week" keyword))
                    (org-agenda-files (pandaye/org-agenda-files-with-archives))
                    (org-agenda-skip-function #'pandaye/org-agenda-skip-not-updated-last-week)
                    (org-agenda-prefix-format '((todo . " %(pandaye/org-agenda-state-change-time-prefix) %-12:c")))
                    (org-agenda-sorting-strategy '(time-down priority-down category-keep)))))
          (pandaye/org-all-todo-keywords)))

(defun pandaye/org-this-week-closed-blocks ()
  "Return agenda blocks for items closed this week, grouped by state."
  (mapcar (lambda (keyword)
            `(todo ,keyword
                   ((org-agenda-overriding-header ,(format "%s Closed This Week" keyword))
                    (org-agenda-files (pandaye/org-agenda-files-with-archives))
                    (org-agenda-skip-function #'pandaye/org-agenda-skip-not-closed-this-week)
                    (org-agenda-prefix-format '((todo . " %(pandaye/org-agenda-closed-time-prefix) %-12:c")))
                    (org-agenda-sorting-strategy '(time-down priority-down category-keep)))))
          (pandaye/org-closed-keywords)))

(defun pandaye/org-agenda-state-change-time-prefix ()
  "Return a formatted last state change timestamp for agenda prefixes."
  (let ((state-change-time (pandaye/org-entry-last-state-change-time
                            (save-excursion (org-end-of-subtree t)))))
    (if state-change-time
        (format-time-string "%m-%d %a %H:%M " state-change-time)
      "")))

(defun pandaye/org-agenda-closed-time-prefix ()
  "Return a formatted closing timestamp for agenda prefixes."
  (let ((closed-time (pandaye/org-entry-closed-time
                      (save-excursion (org-end-of-subtree t)))))
    (if closed-time
        (format-time-string "%m-%d %a %H:%M " closed-time)
      "")))

(with-eval-after-load 'org-agenda
  ;; Use a quieter separator in multi-block agenda views.
  (setq org-agenda-block-separator ?─)
  (set-face-attribute 'org-time-grid nil :foreground "#665c54")
  ;; 本周未来几天的 SCHEDULED 任务用低调的灰青色（与 org-date 同色），
  ;; 避免比今天的任务（org-scheduled-today 亮蓝）更显眼。
  (set-face-attribute 'org-scheduled nil :foreground "#6f8f8f")
  (add-to-list 'org-agenda-custom-commands
               '("w" "Weekly Review"
                 ((agenda "" ((org-agenda-span 'week)
                              (org-agenda-start-on-weekday 1)))
                  (todo "REVIEWING"
                        ((org-agenda-overriding-header "Tasks in Review")
                         (org-agenda-skip-function #'pandaye/org-agenda-skip-planned-outside-week)))
                  (todo "PROCESSING"
                        ((org-agenda-overriding-header "In Progress")
                         (org-agenda-skip-function #'pandaye/org-agenda-skip-planned-outside-week)))
                  (todo "TODO"
                        ((org-agenda-overriding-header "Todo Items")
                         (org-agenda-skip-function #'pandaye/org-agenda-skip-planned-outside-week)))
                  (todo "BLOCK"
                        ((org-agenda-overriding-header "Blocked Tasks")
                         (org-agenda-skip-function #'pandaye/org-agenda-skip-planned-outside-week)))
                  (todo "LATER"
                        ((org-agenda-overriding-header "Scheduled for Later")
                         (org-agenda-skip-function #'pandaye/org-agenda-skip-planned-outside-week))))))
  (add-to-list 'org-agenda-custom-commands
               `("W" "Last Week Status Updates"
                 ,(pandaye/org-last-week-status-update-blocks)))
  (add-to-list 'org-agenda-custom-commands
               `("C" "This Week Closed"
                 ,(pandaye/org-this-week-closed-blocks))))

;; ============================================================
;; 快捷键
;; ============================================================

;; ============================================================
;; Agenda 着色
;; ============================================================

(defun pandaye/org-agenda-colorize-category ()
  "Colorize only the category part of agenda items based on source file."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((cat (get-text-property (point) 'org-category))
             (color (when cat
                      (cl-loop for (pattern . col) in pandaye/org-agenda-file-colors
                                when (string-match-p pattern cat)
                                return col))))
        (when color
          (let ((inhibit-read-only t)
                (line-end (line-end-position)))
            (when (re-search-forward (concat "^[[:space:]]*\\(" (regexp-quote cat) "\\):?") line-end t)
              (add-face-text-property (match-beginning 1) (match-end 1)
                                      `(:foreground ,color))))))
      (forward-line 1))))

(defvaralias 'my/org-agenda-file-colors 'pandaye/org-agenda-file-colors)

(defvar pandaye/org-agenda-file-colors
  '(("project" . "#ff7b72")
    ("issue" . "#79c0ff")
    ("daily" . "#a5d6ff"))
  "文件名到颜色的映射，用于 agenda 条目分类着色。")

(defun pandaye/org-agenda-dim-block-separators ()
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

(add-hook 'org-agenda-finalize-hook #'pandaye/org-agenda-colorize-category)
(add-hook 'org-agenda-finalize-hook #'pandaye/org-agenda-dim-block-separators)

(require 'org-diary)
(setq daily-diary-base-path (expand-file-name "daily" org-base-path))

(provide 'init-org-gtd)
;;; init-org-gtd.el ends here
