;;; init-environment.el --- Configuration paths and environment -*- lexical-binding: t; -*-

;;; Commentary:
;; Establishes the load path shared by all configuration layers.

;;; Code:

(dolist (directory '("core" "modules" "features"))
  (add-to-list 'load-path
               (expand-file-name directory
                                 (expand-file-name "lisp" user-emacs-directory))))

(provide 'init-environment)
;;; init-environment.el ends here
