;;; architecture-test.el --- Configuration architecture constraints -*- lexical-binding: t; -*-

;;; Commentary:
;; Executable constraints for the configuration's core/modules/features split.

;;; Code:

(require 'ert)

(defconst pandaye/test-config-directories
  '("lisp/core" "lisp/modules" "lisp/features")
  "Configuration directories which must be present on `load-path'.")

(ert-deftest architecture-loads-through-init-config ()
  (should (featurep 'init-config))
  (should-not (featurep 'my-config)))

(ert-deftest architecture-adds-each-layer-to-load-path ()
  (dolist (directory pandaye/test-config-directories)
    (should (member (expand-file-name directory user-emacs-directory)
                    load-path))))

(provide 'architecture-test)
;;; architecture-test.el ends here
