;;; architecture-test.el --- Configuration architecture constraints -*- lexical-binding: t; -*-

;;; Commentary:
;; Executable constraints for the configuration's core/modules/features split.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

(defconst pandaye/test-config-directories
  '("lisp/core" "lisp/modules" "lisp/features")
  "Configuration directories which must be present on `load-path'.")

(ert-deftest architecture-loads-through-entry-point ()
  (should (featurep 'init))
  (should-not (featurep 'my-config)))

(ert-deftest architecture-adds-each-layer-to-load-path ()
  (dolist (directory pandaye/test-config-directories)
    (should (member (expand-file-name directory user-emacs-directory)
                    load-path))))

(ert-deftest architecture-registers-each-layer-once ()
  ;; 每个层目录只应被登记一次。
  (dolist (directory pandaye/test-config-directories)
    (let ((expanded (expand-file-name directory user-emacs-directory)))
      (should (= 1 (seq-count (lambda (entry) (equal entry expanded))
                              load-path))))))

(ert-deftest architecture-load-path-entries-exist ()
  ;; 入口曾挂过一个并不存在的 `opensource/'；这里防止再次出现死路径。
  (let ((root (file-name-as-directory (expand-file-name user-emacs-directory))))
    (dolist (entry load-path)
      (when (and (stringp entry)
                 (string-prefix-p root (expand-file-name entry)))
        (should (file-directory-p entry))))))

(ert-deftest architecture-entry-loads-every-layer-module ()
  ;; 入口是唯一组合根：模块清单必须显式写在 init.el 里，便于一眼看到顺序。
  (let ((contents (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "init.el" user-emacs-directory))
                    (buffer-string))))
    (dolist (module '(init-ui init-tui init-gui init-defaults init-local
                      init-editor init-completion init-navigation init-vcs
                      init-org init-org-writing init-org-gtd init-org-roam
                      init-markdown init-input-method init-development
                      init-lsp init-reader init-dsh init-keybindings))
      (should (string-match-p (format "(require '%s)" module) contents)))))

(ert-deftest architecture-features-have-no-configuration-dependencies ()
  (dolist (file (directory-files-recursively
                 (expand-file-name "lisp/features" user-emacs-directory)
                 "[.]el\\'"))
    (let ((contents (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
      (should-not (string-match-p "(use-package\\_>" contents))
      (should-not (string-match-p "(global-set-key\\_>" contents))
      (should-not (string-match-p "(with-eval-after-load\\_>" contents))
      (should-not (string-match-p "(require '[[:space:]]*init-" contents)))))

(ert-deftest architecture-canonical-symbols-have-owned-prefixes ()
  (dolist (file (directory-files-recursively
                 (expand-file-name "lisp" user-emacs-directory)
                 "[.]el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (re-search-forward
        "^(\\(?:defun\\|defcustom\\|defvar\\|defface\\|defgroup\\|define-minor-mode\\)[[:space:]]+my[-/]"
        nil t)))))

(ert-deftest architecture-modules-do-not-load-each-other ()
  (dolist (file (directory-files-recursively
                 (expand-file-name "lisp/modules" user-emacs-directory)
                 "[.]el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "(require '[[:space:]]*init-" nil t)))))

(ert-deftest architecture-centralizes-global-keybindings ()
  (let ((owner (expand-file-name "lisp/core/init-keybindings.el"
                                  user-emacs-directory)))
    (dolist (file (directory-files-recursively
                   (expand-file-name "lisp" user-emacs-directory)
                   "[.]el\\'"))
      (unless (equal file owner)
        (with-temp-buffer
          (insert-file-contents file)
          (should-not (search-forward "(global-set-key" nil t)))))))

(ert-deftest architecture-feature-matches-file-name ()
  (dolist (directory '("lisp/core" "lisp/modules" "lisp/features"))
    (dolist (file (directory-files-recursively
                   (expand-file-name directory user-emacs-directory)
                   "[.]el\\'"))
      (let ((feature (intern (file-name-base file))))
        (with-temp-buffer
          (insert-file-contents file)
          (should (re-search-forward
                   (format "^(provide '%s)"
                           (regexp-quote (symbol-name feature)))
                   nil t)))))))

(provide 'architecture-test)
;;; architecture-test.el ends here
