;;; org-ssh-test.el --- Tests for org-ssh helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-ssh)

(ert-deftest org-ssh-insert-link-and-open-inserts-link-and-opens-host ()
  (let (opened-host)
    (cl-letf (((symbol-function 'my/tmux-ssh-connect-simple)
               (lambda (host)
                 (setq opened-host host))))
      (with-temp-buffer
        (my/org-ssh-insert-link-and-open "10.0.0.1" "node-1")
        (should (equal (buffer-string) "| [[ssh:10.0.0.1][node-1]]\n  "))
        (should (equal opened-host "10.0.0.1"))))))

(ert-deftest org-ssh-insert-link-and-open-uses-host-as-default-name ()
  (let (opened-host)
    (cl-letf (((symbol-function 'my/tmux-ssh-connect-simple)
               (lambda (host)
                 (setq opened-host host))))
      (with-temp-buffer
        (my/org-ssh-insert-link-and-open "10.0.0.2" "")
        (should (equal (buffer-string) "| [[ssh:10.0.0.2][10.0.0.2]]\n  "))
        (should (equal opened-host "10.0.0.2"))))))

(provide 'org-ssh-test)

;;; org-ssh-test.el ends here
