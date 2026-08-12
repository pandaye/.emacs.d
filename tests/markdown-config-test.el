;;; markdown-config-test.el --- Markdown configuration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-mode)

(ert-deftest markdown-config-keeps-intraword-underscore-literal ()
  (with-temp-buffer
    (markdown-mode)
    (insert "snake_case")
    (font-lock-ensure)
    (goto-char 6)
    (should-not (eq (get-text-property (point) 'face)
                    'markdown-italic-face))))

(ert-deftest markdown-config-installs-compatible-advice ()
  (should (advice-member-p
           #'my-markdown-match-italic-skip-intraword-underscore
           'markdown-match-italic)))

(provide 'markdown-config-test)
;;; markdown-config-test.el ends here
