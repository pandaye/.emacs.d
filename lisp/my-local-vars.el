;; -*- lexical-binding: t; -*-
;;; my-local-vars.el --- Load untracked local variables -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads a single untracked local variables file during startup.

;;; Code:

(defgroup my-local-vars nil
  "Startup loader for untracked local variables."
  :group 'convenience)

(defcustom my/local-vars-file
  (expand-file-name "local-vars.local.el" user-emacs-directory)
  "Untracked local variables file loaded at startup."
  :type 'file
  :group 'my-local-vars)

(defcustom my/local-vars-fallback-files
  (list (expand-file-name "common-dirs.local.el" user-emacs-directory))
  "Fallback untracked local files to load when `my/local-vars-file' is absent."
  :type '(repeat file)
  :group 'my-local-vars)

(defun my/load-local-vars ()
  "Load `my/local-vars-file' if it exists."
  (interactive)
  (cond
   ((file-exists-p my/local-vars-file)
    (load-file my/local-vars-file)
    (message "Loaded local vars from %s" my/local-vars-file))
   ((seq-find #'file-exists-p my/local-vars-fallback-files)
    (let ((fallback (seq-find #'file-exists-p my/local-vars-fallback-files)))
      (load-file fallback)
      (message "Loaded fallback local vars from %s" fallback)))
   (t
    (message "Local vars file not found: %s" my/local-vars-file))))

(my/load-local-vars)
(provide 'my-local-vars)
;;; my-local-vars.el ends here
