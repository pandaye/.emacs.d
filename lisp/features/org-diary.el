;; -*- lexical-binding: t; -*-
;;; my-diary.el --- 日记系统配置

;;; Commentary:
;; 每日日记文件管理，包括打开当日日记、按日期打开、列出日记文件。
;; 依赖 my-gtd.el 中的 org-base-path 变量。

;;; Code:

(require 'org-listing)

(defvar daily-diary-base-path nil
  "日记文件的基础路径，格式为 daily/YY/MM-DD.org。")

(defun open-today-diary ()
  "打开今日日记文件，格式为 /path/to/daily/YY/MM-DD.org"
  (interactive)
  (let* ((today (current-time))
         (year (format-time-string "%y" today))
         (month-day (format-time-string "%m-%d" today))
         (diary-dir (expand-file-name year daily-diary-base-path))
         (diary-file (expand-file-name (concat month-day ".org") diary-dir)))

    ;; Create directory if it doesn't exist
    (unless (file-exists-p diary-dir)
      (make-directory diary-dir t))
    ;; Open the diary file
    (find-file diary-file)
    ;; If it's a new file, add a basic header
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Diary - %s\n"
                      (format-time-string "%Y-%m-%d %A" today)))
      (insert (format "#+DATE: %s\n\n"
                      (format-time-string "%Y-%m-%d" today)))
      (insert "* Today's Notes\n\n")
      (save-buffer))))

(defun open-diary-by-date (date-string)
  "打开指定日期的日记文件。
DATE-STRING 格式为 YYYY-MM-DD 或 MM-DD（默认当年）。"
  (interactive "sEnter date (YYYY-MM-DD or MM-DD): ")
  (let* ((parsed-date (if (string-match "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" date-string)
                          ;; MM-DD format, use current year
                          (concat (format-time-string "%Y-") date-string)
                        ;; Assume YYYY-MM-DD format
                        date-string))
         (date-time (date-to-time (concat parsed-date " 00:00:00")))
         (year (format-time-string "%y" date-time))
         (month-day (format-time-string "%m-%d" date-time))
         (diary-dir (expand-file-name year daily-diary-base-path))
         (diary-file (expand-file-name (concat month-day ".org") diary-dir)))
    ;; Create directory if it doesn't exist
    (unless (file-exists-p diary-dir)
      (make-directory diary-dir t))
    ;; Open the diary file
    (find-file diary-file)
    ;; If it's a new file, add a basic header
    (when (= (buffer-size) 0)
      (insert (format "#+TITLE: Daily Diary - %s\n"
                      (format-time-string "%Y-%m-%d %A" date-time)))
      (insert (format "#+DATE: %s\n\n"
                      (format-time-string "%Y-%m-%d" date-time)))
      (insert "* Today's Notes\n\n")
      (save-buffer))))

(defun my/diary--file-date (file)
  "Return encoded time for diary FILE.
Expected layout is daily/YY/MM-DD.org."
  (let* ((year (concat "20" (file-name-nondirectory
                             (directory-file-name
                              (file-name-directory file)))))
         (month-day (file-name-base file))
         (month (substring month-day 0 2))
         (day (substring month-day 3 5)))
    (encode-time 0 0 0
                 (string-to-number day)
                 (string-to-number month)
                 (string-to-number year))))

(defun my/diary--list-items ()
  "Return diary listing items sorted by diary date descending."
  (let ((files (if (file-directory-p daily-diary-base-path)
                   (directory-files-recursively daily-diary-base-path "\\.org\\'")
                 nil)))
    (mapcar (lambda (file)
              (let ((date (my/diary--file-date file)))
                (list :group (my/org-list-group-label date)
                      :file file
                      :date date
                      :label (format-time-string "%Y-%m-%d %a" date))))
            (seq-sort (lambda (a b)
                        (time-less-p (my/diary--file-date b)
                                     (my/diary--file-date a)))
                      files))))

(defun my/diary--insert-list-item (item)
  "Insert one diary ITEM into the current listing buffer."
  (insert (format "- %s\n"
                  (my/org-list-make-link (plist-get item :file)
                                         (plist-get item :label)))))

(defun list-diary-files ()
  "列出所有日记文件，按年月分组并分页显示。"
  (interactive)
  (my/org-list-open-buffer "*Diary Files*"
                           "Daily Diary Files"
                           #'my/diary--list-items
                           #'my/diary--insert-list-item))

(provide 'org-diary)
;;; my-diary.el ends here
