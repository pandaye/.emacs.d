;;; subtle-delimiter.el --- Subtle delimiter faces  -*- lexical-binding: t; -*-

;;; Commentary:
;; Lisp 系语言里把括号弱化成灰色，避免层级颜色过于抢眼。

;;; Code:

(defface subtle-delimiter-face
  '((t :inherit shadow :weight normal))
  "Face for visually de-emphasized delimiters.")

(defvaralias 'my-subtle-delimiter-font-lock-keywords
  'subtle-delimiter-font-lock-keywords)

(defvar subtle-delimiter-font-lock-keywords
  `((,(regexp-opt '("(" ")" "[" "]" "{" "}")) . 'subtle-delimiter-face))
  "Font lock keywords for `subtle-delimiter-mode'.")

(define-minor-mode subtle-delimiter-mode
  "De-emphasize delimiters with a subtle face."
  :lighter nil
  (if subtle-delimiter-mode
      (font-lock-add-keywords nil subtle-delimiter-font-lock-keywords 'append)
    (font-lock-remove-keywords nil subtle-delimiter-font-lock-keywords))
  (when font-lock-mode
    (font-lock-flush)))

(put 'my-subtle-delimiter-face 'face-alias 'subtle-delimiter-face)
(defalias 'my-subtle-delimiter-mode #'subtle-delimiter-mode)

(provide 'subtle-delimiter)
;;; subtle-delimiter.el ends here
