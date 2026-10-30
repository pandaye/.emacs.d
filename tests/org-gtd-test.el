;;; org-gtd-test.el --- Tests for GTD agenda helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'init-org-gtd)

(defun org-gtd-test-skip-planned-p (heading-text)
  "Return non-nil if `pandaye/org-agenda-skip-planned-outside-week'
skips the entry built from HEADING-TEXT."
  (with-temp-buffer
    (org-mode)
    (insert heading-text)
    (goto-char (point-min))
    (org-back-to-heading)
    (not (null (pandaye/org-agenda-skip-planned-outside-week)))))

(defun org-gtd-test-format-date (time)
  "Format TIME as an Org date string."
  (format-time-string "%Y-%m-%d %a" time))

(ert-deftest org-gtd-skip-planned-outside-week ()
  (let* ((range (pandaye/org-this-week-range))
         (monday (car range))
         (next-monday (cdr range))
         (next-tuesday (time-add next-monday (days-to-time 1)))
         (last-tuesday (time-subtract monday (days-to-time 6))))
    (should-not
     (org-gtd-test-skip-planned-p
      (format "* TODO Task\n  SCHEDULED: <%s>\n" (org-gtd-test-format-date monday))))
    (should-not
     (org-gtd-test-skip-planned-p
      (format "* TODO Task\n  DEADLINE: <%s>\n" (org-gtd-test-format-date monday))))
    (should
     (org-gtd-test-skip-planned-p
      (format "* TODO Task\n  SCHEDULED: <%s>\n" (org-gtd-test-format-date next-tuesday))))
    (should
     (org-gtd-test-skip-planned-p
      (format "* TODO Task\n  DEADLINE: <%s>\n" (org-gtd-test-format-date last-tuesday))))
    (should-not
     (org-gtd-test-skip-planned-p "* TODO Task\n  Some text.\n"))))

(ert-deftest org-gtd-weekly-review-filters-planned-outside-week ()
  (let ((command (assoc "w" org-agenda-custom-commands)))
    (should command)
    (dolist (block (caddr command))
      (when (eq (car block) 'todo)
        (let ((settings (caddr block)))
          (should (member
                   (list 'org-agenda-skip-function
                         (list 'function 'pandaye/org-agenda-skip-planned-outside-week))
                   settings)))))))

;;; org-gtd-test.el ends here
