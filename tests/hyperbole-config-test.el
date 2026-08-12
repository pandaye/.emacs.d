;;; hyperbole-config-test.el --- Tests for Hyperbole key configuration -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(defvar hbmap:dir-user)

(ert-deftest hyperbole-config-lazy-action-key-on-meta-o ()
  "M-o should lazy-load Hyperbole without taking over Org M-RET."
  (should-not (featurep 'hyperbole))
  (should (eq (key-binding (kbd "M-o") t)
              #'pandaye/editor-hyperbole-action-key))
  (with-temp-buffer
    (org-mode)
    (should (eq (key-binding (kbd "M-RET")) #'org-meta-return)))
  (let ((hbmap:dir-user (make-temp-file "hyperbole-user" t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (condition-case err
                (call-interactively #'pandaye/editor-hyperbole-action-key)
              (error
               (should (string-match-p "No action defined"
                                       (error-message-string err))))))
          (should (featurep 'hyperbole))
          (should (bound-and-true-p hyperbole-mode))
          (should (eq (lookup-key hyperbole-mode-map (kbd "M-o")) #'hkey-either)))
      (delete-directory hbmap:dir-user t))))

(provide 'hyperbole-config-test)

;;; hyperbole-config-test.el ends here
