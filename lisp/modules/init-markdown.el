;;; init-markdown.el --- Markdown and GFM configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Owns Markdown package lifecycle, display, and compatibility advice.

;;; Code:

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-fontify-code-block-default-mode 'fundamental-mode)
  :config
  (defun my-markdown-match-italic-skip-intraword-underscore (orig-fun last)
    "Make `markdown-mode' ignore intraword underscore emphasis like GFM."
    (let (found done)
      (while (and (not done) (funcall orig-fun last))
        (if (and (not (derived-mode-p 'gfm-mode))
                 (eq (char-after (match-beginning 0)) ?_)
                 (not (markdown--gfm-markup-underscore-p
                       (match-beginning 0)
                       (match-end 3))))
            (progn
              (goto-char (min (1+ (match-beginning 0)) last))
              (unless (< (point) last)
                (setq done t)))
          (setq found t
                done t)))
      found))

  (when (and (fboundp 'markdown--gfm-markup-underscore-p)
             (not (advice-member-p
                   #'my-markdown-match-italic-skip-intraword-underscore
                   'markdown-match-italic)))
    (advice-add 'markdown-match-italic
                :around #'my-markdown-match-italic-skip-intraword-underscore))

  (set-face-attribute 'markdown-code-face nil
                      :background "#32302f"
                      :extend t)
  (set-face-attribute 'markdown-pre-face nil
                      :background "#32302f"
                      :extend t)
  (set-face-attribute 'markdown-inline-code-face nil
                      :inherit '(font-lock-constant-face)
                      :background 'unspecified)
  (set-face-attribute 'markdown-language-keyword-face nil
                      :background "#32302f"
                      :foreground "gray35")
  (set-face-attribute 'markdown-language-info-face nil
                      :background "#32302f"
                      :foreground "gray35"))

(provide 'init-markdown)
;;; init-markdown.el ends here
