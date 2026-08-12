;; -*- lexical-binding: t; -*-
;;; common-dirs.el --- Quick launcher for common directories

;;; Commentary:
;; Use preloaded local variables containing named directories,
;; then start file selection rooted at the chosen directory.

;;; Code:

(require 'subr-x)

(defgroup common-dirs nil
  "Quick launcher for common directories."
  :group 'convenience)

(defvaralias 'my/common-dirs-alist 'common-dirs-alist)

(defvar common-dirs-alist nil
  "Alist of common directories.
Each entry is (NAME . PATH). NAME is shown in completion, PATH is used as picker root.")

(defun common-dirs--candidates ()
  "Return normalized candidates from `common-dirs-alist'."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (expand-file-name (cdr entry))))
          common-dirs-alist))

(defun common-dirs--read-directory ()
  "Prompt for and return a configured directory path."
  (interactive)
  (unless common-dirs-alist
    (user-error "`common-dirs-alist' is empty; set it in local-vars.local.el"))
  (let* ((candidates (common-dirs--candidates))
          (choice (completing-read "Base directory: " (mapcar #'car candidates) nil t)))
    (unless (and choice (not (string-empty-p choice)))
      (user-error "No directory selected"))
    (let ((target (cdr (assoc choice candidates))))
      (unless target
        (user-error "Unknown directory: %s" choice))
      (unless (file-directory-p target)
        (user-error "Directory does not exist: %s" target))
      target)))

(defun common-dirs-find-file ()
  "Choose a configured directory, then start file selection rooted there."
  (interactive)
  (let ((default-directory (file-name-as-directory (common-dirs--read-directory))))
    (cond
     ((fboundp 'helm-find-files)
      (helm-find-files-1 default-directory))
     (t
      (call-interactively #'find-file)))))

(defalias 'my/common-dirs-find-file #'common-dirs-find-file)

(provide 'common-dirs)
;;; common-dirs.el ends here
