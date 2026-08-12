;;; run-tests.el --- Run configuration tests -*- lexical-binding: t; -*-

(load (expand-file-name "config-load-test.el" (file-name-directory load-file-name)))
(load (expand-file-name "config-contract-test.el" (file-name-directory load-file-name)))
(load (expand-file-name "architecture-test.el" (file-name-directory load-file-name)))
(load (expand-file-name "hyperbole-config-test.el" (file-name-directory load-file-name)))
(load (expand-file-name "markdown-config-test.el" (file-name-directory load-file-name)))
(load (expand-file-name "static-blog-test.el" (file-name-directory load-file-name)))
(load (expand-file-name "org-ssh-test.el" (file-name-directory load-file-name)))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
