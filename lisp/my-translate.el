;; -*- lexical-binding: t; -*-
;;; my-translate.el --- Translation helpers for reading buffers

;;; Commentary:
;; 为阅读场景提供一个就地翻译命令，使用 gt 进行翻译，
;; 优先选择本地 StarDict 词典（sdcv），找不到时回退到 Google 在线翻译。

;;; Code:

(require 'subr-x)
(require 'json)

(defconst my/gt-reading-langs '(en zh)
  "Default languages used by reading translation commands.")

(defvar my/gt-wordbook-file (locate-user-emacs-file "gt-wordbook.txt")
  "File used to store words looked up during reading.")

(defvar my/gt-stardict-dir (expand-file-name "~/.stardict/dic")
  "Directory containing local StarDict dictionaries for sdcv.")

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

(defun my/gt--sdcv-lookup (text)
  "Run sdcv for TEXT and return parsed JSON entries, or nil on failure."
  (and (executable-find "sdcv")
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
               (json-read)
             (error nil))))))

(defun my/gt--exact-word-match-p (entries word)
  "Return non-nil when JSON ENTRIES has exact WORD match (case-insensitive)."
  (when (sequencep entries)
    (let ((needle (downcase word)))
      (catch 'found
        (mapc (lambda (entry)
                (let ((entry-word (cdr (assq 'word entry))))
                  (when (and (stringp entry-word)
                             (string= needle (downcase entry-word)))
                    (throw 'found t))))
              entries)
        nil))))

(defun my/gt--local-dictionary-hit-p (text)
  "Return non-nil when local sdcv dictionaries can explain TEXT."
  (and (my/gt--wordbook-eligible-p text)
       (my/gt--exact-word-match-p (my/gt--sdcv-lookup text) text)))

(defun my/gt--render ()
  "Build a gt renderer using pop-to-buffer below the selected window."
  (gt-buffer-render
   :name "*gt-reading*"
   :window-config '((display-buffer-below-selected))))

(defun my/gt--stardict-entry-text (entry dict)
  "Extract definition text from ENTRY for DICT, pretty-printed if available."
  (let* ((raw (cdr (assq 'definition entry)))
         (def (if (and (fboundp 'gt-stardict-pretty-definition) raw)
                  (gt-stardict-pretty-definition (intern dict) raw)
                raw)))
    (when (and (stringp def) (not (string-empty-p def)))
      def)))

(defun my/gt--clean-stardict-definition (text definition)
  "Clean and indent a StarDict DEFINITION for TEXT.

Lines matching a label pattern (\"word :\", \"word 1:\", \"1:\")
are treated as labeled definitions, with labels right-aligned to
a fixed width of 6 so all colons line up.  Lines indented deeper
than the current section are treated as continuations."
  (let ((needle (downcase (string-trim text)))
        (label-re "\\`\\([[:alpha:]]+ \\(?:[0-9]+\\)?\\|[0-9]+\\): ")
        (label-width 6)
        result
		section-indent)
    (dolist (line (split-string (or definition "") "\n"))
      (let* ((trimmed (string-trim line))
             (raw-indent (- (length line) (length trimmed))))
        (cond
         ;; Skip blank lines and the headword itself. 
         ((or (string-empty-p trimmed)
              (string= needle (downcase trimmed))))
         ;; Labeled definition: right-align label to LABEL-WIDTH.
         ((string-match label-re trimmed)
          (let* ((label (match-string 1 trimmed))
                 (pad   (max 0 (- label-width (length label)))))
            (setq section-indent raw-indent)
            (push (concat "  " (make-string pad ?\s) label
                          (substring trimmed (match-end 1)))
                  result)))
         ;; Continuation of the current section.
         ((and section-indent (> raw-indent section-indent))
          (push (concat "          " trimmed) result))
         ;; Plain new section.
         (t
          (setq section-indent raw-indent)
          (push (concat "    " trimmed) result)))))
    (string-join (nreverse result) "\n")))


(defun my/gt--format-stardict-json (text entries)
  "Format sdcv JSON ENTRIES for TEXT as grouped dictionary blocks.

Group entries by dict name, extract+clean each definition, then
assemble per-dictionary blocks separated by blank lines."
  (let ((by-dict (seq-group-by
                  (lambda (e) (or (cdr (assq 'dict e)) "StarDict"))
                  entries))
        blocks)
    (dolist (group by-dict)
      (let* ((dict (car group))
             (defs (delq nil
                         (mapcar (lambda (e)
                                   (let ((cleaned (my/gt--clean-stardict-definition
                                                   text
                                                   (my/gt--stardict-entry-text e dict))))
                                     (unless (string-empty-p cleaned)
                                       cleaned)))
                                 (cdr group)))))
        (when defs
          (push (concat dict "\n\n" (string-join defs "\n")) blocks))))
    (string-join (nreverse blocks) "\n\n")))

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

(defun my/gt--choose-translator (text)
  "Pick the appropriate translator for TEXT.
Returns a local StarDict translator when TEXT is a single word that
exists in local dictionaries; otherwise returns a Google translator."
  (if (and (my/gt--wordbook-eligible-p text)
           (my/gt--local-dictionary-hit-p text))
      (my/gt--dictionary-translator text)
    (my/gt--google-translator text)))

(defun my/gt-translate-dwim ()
  "Translate active region or word at point.
Prompt when there is no obvious text under point."
  (interactive)
  (require 'gt)
  (let ((text (or (my/gt--selection-or-word)
                  (read-string "Translate: "))))
    (my/gt-record-word-to-wordbook text)
    (gt-start (my/gt--choose-translator text))))

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
