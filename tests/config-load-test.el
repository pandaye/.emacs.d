;;; config-load-test.el --- Configuration smoke tests -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest config-loads-current-entrypoint ()
  (should (featurep 'init))
  (should (featurep 'init-config))
  (should-not (featurep 'my-config))
  (dolist (feature '(init-ui init-editor init-completion init-navigation
                     init-vcs my-writing my-development my-reader
                     init-keybindings))
    (should (featurep feature)))
  (should-not (featurep 'pandaye-init)))

(ert-deftest config-keeps-core-keybindings ()
  (should (eq (key-binding (kbd "C-c f s")) #'save-buffer))
  (should (eq (key-binding (kbd "C-c w w")) #'delete-other-windows))
  (should (eq (key-binding (kbd "C-c i i")) #'toggle-input-method))
  (should (eq (key-binding (kbd "C-c d d")) #'tmux-manager-switch-to-buffer)))

(ert-deftest config-does-not-load-removed-integrations ()
  (dolist (feature '(org-opencode my-gptel my-agent-shell emux))
    (should-not (featurep feature))))

(provide 'config-load-test)
;;; config-load-test.el ends here
