;;; my-subtle-delimiter.el --- Subtle delimiter faces  -*- lexical-binding: t; -*-

;;; Commentary:
;; Lisp 系语言里把括号弱化成灰色，避免层级颜色过于抢眼。

;;; Code:

(defface my-subtle-delimiter-face
  '((t :inherit shadow :weight normal))
  "Face for visually de-emphasized delimiters.")

(defvar my-subtle-delimiter-font-lock-keywords
  `((,(regexp-opt '("(" ")" "[" "]" "{" "}")) . 'my-subtle-delimiter-face))
  "Font lock keywords for `my-subtle-delimiter-mode'.")

(define-minor-mode my-subtle-delimiter-mode
  "De-emphasize delimiters with a subtle face."
  :lighter nil
  (if my-subtle-delimiter-mode
      (font-lock-add-keywords nil my-subtle-delimiter-font-lock-keywords 'append)
    (font-lock-remove-keywords nil my-subtle-delimiter-font-lock-keywords))
  (when font-lock-mode
    (font-lock-flush)))

(dolist (hook '(scheme-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                racket-mode-hook
                clojure-mode-hook))
  (add-hook hook #'my-subtle-delimiter-mode))

(provide 'subtle-delimiter)
;;; my-subtle-delimiter.el ends here
