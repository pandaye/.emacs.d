(defun my/org-fc-insert-word ()
  (interactive)
  (insert (format "** %s
:PROPERTIES:
:FC_TYPE: twosided
:END:
意思：%s
例句：%s

"
				  (read-string "单词: ")
                  (read-string "中文解释: ")
                  (read-string "例句: "))))

(defun pjx/org-fc-insert-jp-cloze ()
  (interactive)
  (let ((sentence (read-string "句子（用 ____ 表示单词）: "))
        (answer (read-string "答案: ")))
    (insert (format "** %s
:PROPERTIES:
:FC_TYPE: cloze
:END:

%s
{%s}

"
                    answer
                    sentence
                    answer))))

(defun pjx/org-fc-insert-jp-sentence ()
  (interactive)
  (insert (format "** %s
:PROPERTIES:
:FC_TYPE: simple
:END:
中文：%s

"
                  (read-string "句子: ")
                  (read-string "中文翻译: "))))



(use-package hydra)
(use-package org-fc
  :load-path "~/.emacs.d/site-lisp/org-fc"
  :custom (org-fc-directories '("~/.pandaye-journal/words/jp/"))
  :config
  (require 'org-fc-hydra)
  (with-eval-after-load 'org
	(define-key org-mode-map (kbd "C-c k w") #'my/org-fc-insert-word)
	(define-key org-mode-map (kbd "C-c k c") #'my/org-fc-insert-cloze)
	(define-key org-mode-map (kbd "C-c k s") #'my/org-fc-insert-sentence)))

(provide 'my-word)
