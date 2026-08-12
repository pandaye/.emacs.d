;; -*- lexical-binding: t; -*-
;;; my-common-dirs.el --- Quick launcher for common directories

;;; Commentary:
;; Use preloaded local variables containing named directories,
;; then start file selection rooted at the chosen directory.

;;; Code:

(require 'subr-x)

(defgroup my-common-dirs nil
  "Quick launcher for common directories."
  :group 'convenience)

(defvar my/common-dirs-alist nil
  "Alist of common directories.
Each entry is (NAME . PATH). NAME is shown in completion, PATH is used as picker root.")

(defun my/common-dirs--candidates ()
  "Return normalized candidates from `my/common-dirs-alist'."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (expand-file-name (cdr entry))))
          my/common-dirs-alist))

(defun my/common-dirs--read-directory ()
  "Prompt for and return a configured directory path."
  (interactive)
  (unless my/common-dirs-alist
    (user-error "`my/common-dirs-alist' is empty; set it in local-vars.local.el"))
  (let* ((candidates (my/common-dirs--candidates))
          (choice (completing-read "Base directory: " (mapcar #'car candidates) nil t)))
    (unless (and choice (not (string-empty-p choice)))
      (user-error "No directory selected"))
    (let ((target (cdr (assoc choice candidates))))
      (unless target
        (user-error "Unknown directory: %s" choice))
      (unless (file-directory-p target)
        (user-error "Directory does not exist: %s" target))
      target)))

(defun my/common-dirs-find-file ()
  "Choose a configured directory, then start file selection rooted there."
  (interactive)
  (let ((default-directory (file-name-as-directory (my/common-dirs--read-directory))))
    (cond
     ((fboundp 'helm-find-files)
      (helm-find-files-1 default-directory))
     (t
      (call-interactively #'find-file)))))

(provide 'common-dirs)
;;; my-common-dirs.el ends here
