;; -*- lexical-binding: t; -*-
;;; my-translate.el --- Translation helpers for reading buffers

;;; Commentary:
;; 为阅读场景提供一个就地翻译命令：优先用 posframe/child-frame 悬浮显示，
;; 在不可用时自动回退到 buffer 渲染。

;;; Code:

(require 'subr-x)
(require 'json)

(defconst my/gt-reading-langs '(en zh)
  "Default languages used by reading translation commands.")

(defvar my/gt-wordbook-file (locate-user-emacs-file "gt-wordbook.txt")
  "File used to store words looked up during reading.")

(defvar my/gt-stardict-dir (expand-file-name "~/.stardict/dic")
  "Directory containing local StarDict dictionaries for sdcv.")

(use-package posframe
  :defer t)

(use-package gt
  :defer t
  :init
  (setq gt-langs my/gt-reading-langs
        gt-buffer-render-follow-p t))

(defun my/gt--selection-or-word ()
  "Return active region text or word at point, trimmed."
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'word t))))
    (when text
      (let ((trimmed (string-trim text)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun my/gt--wordbook-eligible-p (text)
  "Return non-nil when TEXT looks like a single word worth recording."
  (and text
       (not (string-match-p "[[:space:]]" text))
       (string-match-p "[[:alpha:]]" text)))

(defun my/gt--append-unique-line (file line)
  "Append LINE to FILE once, ignoring case and duplicates."
  (let ((normalized (downcase (string-trim line))))
    (unless (string-empty-p normalized)
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file))
        (goto-char (point-min))
        (unless (re-search-forward (format "^%s$" (regexp-quote normalized)) nil t)
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert normalized "\n")
          (write-region nil nil file nil 'silent))))))

(defun my/gt-record-word-to-wordbook (text)
  "Record TEXT to the reading wordbook when it is a single word."
  (when (my/gt--wordbook-eligible-p text)
    (my/gt--append-unique-line my/gt-wordbook-file text)))

(defun my/gt--local-dictionary-hit-p (text)
  "Return non-nil when local sdcv dictionaries can explain TEXT."
  (and (my/gt--wordbook-eligible-p text)
       (executable-find "sdcv")
       (with-temp-buffer
         (when (eq 0 (call-process "sdcv" nil t nil
                                   "--non-interactive"
                                   "--json-output"
                                   "-0"
                                   "-1"
                                   "--data-dir"
                                   my/gt-stardict-dir
                                   "--only-data-dir"
                                   text))
            (goto-char (point-min))
            (condition-case nil
                (let* ((needle (downcase text))
                       (entries (json-read)))
                  (catch 'exact-match
                    (mapc (lambda (entry)
                            (let ((word (cdr (assq 'word entry))))
                              (when (and (stringp word)
                                         (string= needle (downcase word)))
                                (throw 'exact-match t))))
                          entries)
                    nil))
              (error nil))))))

(defun my/gt--posframe-available-p ()
  "Return non-nil when posframe can be used for gt rendering."
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)))

(defun my/gt--render ()
  "Build a gt renderer suitable for the current frame type."
  (gt-buffer-render
   :name "*gt-reading*"
   :window-config '((display-buffer-below-selected))))

(defun my/gt--simplify-stardict-result (text result)
  "Remove redundant headword-only lines for TEXT from StarDict RESULT."
  (let* ((needle (downcase (string-trim text)))
         (lines (split-string result "\n"))
         (filtered
          (seq-remove
           (lambda (line)
             (string= needle (downcase (string-trim line))))
           lines)))
    (string-join filtered "\n")))

(defun my/gt--clean-stardict-definition (text definition)
  "Trim and normalize StarDict DEFINITION for TEXT."
  (let ((needle (downcase (string-trim text)))
        cleaned
        section-type)
    (dolist (line (split-string (or definition "") "\n")
				  (string-join (nreverse cleaned) "\n"))
      (let ((trimmed (string-trim line)))
        (unless (or (string-empty-p trimmed)
                    (string= needle (downcase trimmed)))
          (let ((indent-level
                 (cond
                  ((string-match-p "\\`[[:alpha:]]+ [0-9]+:" trimmed)
                   (setq section-type 'numbered)
                   0)
                  ((string-match-p "\\`[0-9]+:" trimmed)
                   (setq section-type 'numbered)
                   2)
                  ((or (string-match-p "\\`\[[^]]+\]\\'" trimmed)
                       (string-match-p "\\`[[:alpha:]]+\\.\\'" trimmed))
                   (setq section-type 'plain)
                   2)
                  ((eq section-type 'numbered)
				   5)
                  (t
                   (setq section-type 'plain)
                   2))))
            (push (concat "  " (make-string indent-level ? ) trimmed) cleaned)))))))

(defun my/gt--format-stardict-json (text entries)
  "Format sdcv JSON ENTRIES for TEXT as grouped dictionary blocks."
  (let ((groups nil)
        (order nil))
    (mapc (lambda (entry)
            (let* ((dict (or (cdr (assq 'dict entry)) "StarDict"))
                   (definition (my/gt--clean-stardict-definition
                                text
                                (if (and (fboundp 'gt-stardict-pretty-definition)
                                         (cdr (assq 'definition entry)))
                                    (gt-stardict-pretty-definition
                                     (intern dict)
                                     (cdr (assq 'definition entry)))
                                  (cdr (assq 'definition entry))))))
              (when (and (stringp definition)
                         (not (string-empty-p definition)))
                (unless (assoc dict groups)
                  (push dict order)
                  (push (cons dict nil) groups))
                (setcdr (assoc dict groups)
                        (append (cdr (assoc dict groups)) (list definition))))))
          (append entries nil))
    (string-join
     (mapcar (lambda (dict)
               (let ((definitions (cdr (assoc dict groups))))
                 (concat dict "\n\n" (string-join definitions "\n"))))
             (nreverse order))
     "\n\n")))

(defun my/gt--parse-stardict-task (task)
  "Parse local StarDict TASK results with grouped dictionary formatting."
  (with-slots (text res) task
    (let ((word (car text)))
      (setf res
            (mapcar (lambda (item)
                      (my/gt--format-stardict-json
                       word
                       (json-read-from-string item)))
                    res)))))

(defun my/gt--dictionary-translator (text)
  "Build a local dictionary translator for single-word TEXT."
  (gt-translator
    :taker (gt-taker :text text :langs my/gt-reading-langs :pick nil)
    :engines (gt-stardict-engine :dir my/gt-stardict-dir
                                 :dir-only t
                                 :parse #'my/gt--parse-stardict-task)
    :render (my/gt--render)))

(defun my/gt--google-translator (text)
  "Build an online Google translator for TEXT."
  (gt-translator
    :taker (gt-taker :text text :langs my/gt-reading-langs :pick nil)
    :engines (gt-google-engine)
    :render (my/gt--render)))

(defun my/gt-translate-dwim ()
  "Translate active region or word at point.
Prompt when there is no obvious text under point."
  (interactive)
  (require 'gt)
  (let ((text (or (my/gt--selection-or-word)
                  (read-string "Translate: "))))
    (my/gt-record-word-to-wordbook text)
    (gt-start (if (my/gt--wordbook-eligible-p text)
                  (if (my/gt--local-dictionary-hit-p text)
                      (my/gt--dictionary-translator text)
                    (my/gt--google-translator text))
                (my/gt--google-translator text)))))

(defun my/gt-open-wordbook ()
  "Open the reading wordbook file."
  (interactive)
  (find-file my/gt-wordbook-file))

(defun my/elfeed-translate-dwim ()
  "Translate selected text or word at point while reading Elfeed."
  (interactive)
  (my/gt-translate-dwim))

(with-eval-after-load 'elfeed
  (define-key elfeed-show-mode-map (kbd "C-c k") #'my/elfeed-translate-dwim)
  (define-key elfeed-search-mode-map (kbd "C-c k") #'my/elfeed-translate-dwim))

(provide 'my-translate)
;;; my-translate.el ends here
