;;; init-local.el --- Load private local configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Preserves the existing local variables loading point before editor setup.

;;; Code:

(require 'seq)

(defgroup pandaye-local nil
  "Startup loader for untracked local variables."
  :group 'convenience)

(defvaralias 'my/local-vars-file 'pandaye/local-vars-file)
(defvaralias 'my/local-vars-fallback-files
  'pandaye/local-vars-fallback-files)

(defcustom pandaye/local-vars-file
  (expand-file-name "local-vars.local.el" user-emacs-directory)
  "Untracked local variables file loaded at startup."
  :type 'file
  :group 'pandaye-local)

(defcustom pandaye/local-vars-fallback-files
  (list (expand-file-name "common-dirs.local.el" user-emacs-directory))
  "Fallback files loaded when `pandaye/local-vars-file' is absent."
  :type '(repeat file)
  :group 'pandaye-local)

(defun pandaye/load-local-vars ()
  "Load the first available private local variables file."
  (interactive)
  (cond
   ((file-exists-p pandaye/local-vars-file)
    (load-file pandaye/local-vars-file)
    (message "Loaded local vars from %s" pandaye/local-vars-file))
   ((seq-find #'file-exists-p pandaye/local-vars-fallback-files)
    (let ((fallback (seq-find #'file-exists-p
                              pandaye/local-vars-fallback-files)))
      (load-file fallback)
      (message "Loaded fallback local vars from %s" fallback)))
   (t
    (message "Local vars file not found: %s" pandaye/local-vars-file))))

(defalias 'my/load-local-vars #'pandaye/load-local-vars)

(pandaye/load-local-vars)

(provide 'init-local)
;;; init-local.el ends here
