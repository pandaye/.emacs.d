;; -*- lexical-binding: t; -*-
;;; translate.el --- Translation helpers for reading buffers

;;; Commentary:
;; 为阅读场景提供一个就地翻译命令，使用 gt 进行翻译，
;; 优先选择本地 StarDict 词典（sdcv），找不到时回退到 Google 在线翻译。

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'json)
(require 'sqlite)
(require 'org-listing)

(dolist (names '((my/gt-wordbook-db-file . translate-wordbook-db-file)
                 (my/gt-stardict-dir . translate-stardict-dir)
                 (my/gt-sentence-abbreviations . translate-sentence-abbreviations)
                 (my/gt-pysbd-venv-dir . translate-pysbd-venv-dir)
                 (my/gt-pysbd-requirements-file . translate-pysbd-requirements-file)
                 (my/gt-pysbd-helper-file . translate-pysbd-helper-file)
                 (my/gt-pysbd-language . translate-pysbd-language)
                 (my/gt-pysbd-timeout-seconds . translate-pysbd-timeout-seconds)))
  (defvaralias (car names) (cdr names)))

(defvaralias 'my/gt-reading-langs 'translate-reading-langs)

(defconst translate-reading-langs '(en zh)
  "Default languages used by reading translation commands.")

(defvar translate-wordbook-db-file (locate-user-emacs-file "gt-wordbook.sqlite")
  "SQLite database file used to store words looked up during reading.

Set this in local-vars.local.el before loading `my-translate', for example:

  (setq translate-wordbook-db-file \"~/Documents/words.sqlite\")")

(defvar translate-stardict-dir (expand-file-name "~/.stardict/dic")
  "Directory containing local StarDict dictionaries for sdcv.")

(defvar translate-sentence-abbreviations
  '("dr" "mr" "mrs" "ms" "prof" "sr" "jr" "st"
    "e.g" "i.e" "etc" "vs" "fig" "no" "ph.d" "u.s" "u.k")
  "Lowercase abbreviations whose period should not end a context sentence.")

(defvar translate-pysbd-venv-dir
  (expand-file-name ".venv-translate" user-emacs-directory)
  "Virtualenv directory for the PySBD sentence splitter.")

(defvar translate-pysbd-requirements-file
  (expand-file-name "requirements-translate.txt" user-emacs-directory)
  "Requirements file used to install the PySBD sentence splitter.")

(defvar translate-pysbd-helper-file
  (expand-file-name "scripts/pysbd-current-sentence.py" user-emacs-directory)
  "Python helper that returns the PySBD sentence around point.")

(defvar translate-pysbd-language "en"
  "Language code passed to PySBD for sentence segmentation.")

(defvar translate-pysbd-timeout-seconds 2
  "Seconds to wait for the PySBD helper before falling back.")

(defvar translate-pysbd-install-process nil
  "Current background process used to install or check PySBD.")

(defun translate-pysbd-python ()
  "Return the Python executable inside `translate-pysbd-venv-dir'."
  (expand-file-name (if (eq system-type 'windows-nt)
                        "Scripts/python.exe"
                      "bin/python")
                    translate-pysbd-venv-dir))

(defun translate-pysbd-runtime-files-present-p ()
  "Return non-nil when the PySBD runtime files are present."
  (and (file-executable-p (translate-pysbd-python))
       (file-readable-p translate-pysbd-helper-file)))

(defun translate-pysbd--install-running-p ()
  "Return non-nil when a PySBD install/check process is alive."
  (process-live-p translate-pysbd-install-process))

(defun translate-pysbd--install-buffer ()
  "Return the install log buffer."
  (get-buffer-create "*my-translate-pysbd-install*"))

(defun translate-pysbd--start-pip-install ()
  "Install PySBD requirements into the translation virtualenv."
  (if (not (file-readable-p translate-pysbd-requirements-file))
      (progn
        (setq translate-pysbd-install-process nil)
        (message "my-translate PySBD requirements file is missing: %s"
                 translate-pysbd-requirements-file))
    (setq translate-pysbd-install-process
          (make-process
           :name "my-translate-pysbd-pip-install"
           :buffer (translate-pysbd--install-buffer)
           :command (list (translate-pysbd-python) "-m" "pip" "install"
                          "-r" translate-pysbd-requirements-file)
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (setq translate-pysbd-install-process nil)
               (message "my-translate PySBD install %s"
                        (if (and (eq (process-status proc) 'exit)
                                 (zerop (process-exit-status proc)))
                            "finished"
                          "failed; see *my-translate-pysbd-install*"))))))))

(defun translate-pysbd--start-import-check ()
  "Check whether PySBD can be imported, installing it when missing."
  (setq translate-pysbd-install-process
        (make-process
         :name "my-translate-pysbd-check"
         :buffer (translate-pysbd--install-buffer)
         :command (list (translate-pysbd-python) "-c" "import pysbd")
         :noquery t
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (if (and (eq (process-status proc) 'exit)
                      (zerop (process-exit-status proc)))
                 (setq translate-pysbd-install-process nil)
               (translate-pysbd--start-pip-install)))))))

(defun translate-pysbd--start-venv-create ()
  "Create the translation virtualenv, then install PySBD."
  (let ((python (or (executable-find "python3")
                    (executable-find "python"))))
    (if (not python)
        (message "my-translate PySBD install skipped: python3 not found")
      (setq translate-pysbd-install-process
            (make-process
             :name "my-translate-pysbd-venv"
             :buffer (translate-pysbd--install-buffer)
             :command (list python "-m" "venv" translate-pysbd-venv-dir)
             :noquery t
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (if (and (eq (process-status proc) 'exit)
                          (zerop (process-exit-status proc)))
                     (translate-pysbd--start-pip-install)
                   (setq translate-pysbd-install-process nil)
                   (message "my-translate PySBD venv creation failed; see %s"
                            (buffer-name (translate-pysbd--install-buffer)))))))))))

(defun translate-pysbd-ensure-installed ()
  "Ensure the PySBD runtime exists during Emacs configuration startup.

This function starts background processes only; word lookup never
installs Python dependencies."
  (interactive)
  (unless (translate-pysbd--install-running-p)
    (if (file-executable-p (translate-pysbd-python))
        (translate-pysbd--start-import-check)
      (translate-pysbd--start-venv-create))))

(defun translate--selection-or-word ()
  "Return active region text or word at point, trimmed."
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'word t))))
    (when text
      (let ((trimmed (string-trim text)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun translate--wordbook-eligible-p (text)
  "Return non-nil when TEXT looks like a single word worth recording."
  (and text
       (not (string-match-p "[[:space:]]" text))
       (string-match-p "[[:alpha:]]" text)))

(defun translate--period-after-abbreviation-p ()
  "Return non-nil if point is on a period that belongs to an abbreviation."
  (and (eq (char-after) ?.)
       (save-excursion
         (let (token)
           (skip-chars-backward "[:alpha:].")
           (let ((start (point)))
             (skip-chars-forward "[:alpha:].")
             (setq token (downcase (buffer-substring-no-properties start (point)))))
           (setq token (replace-regexp-in-string "\\.+\\'" "" token))
           (or (member token translate-sentence-abbreviations)
               (string-match-p "\\`[[:alpha:]]\\(?:\\.[[:alpha:]]\\)+\\'" token))))))

(defun translate--sentence-boundary-backward ()
  "Return the start position of the sentence around point."
  (save-excursion
    (let ((limit (line-beginning-position))
          found)
      (while (and (not found) (> (point) limit))
        (backward-char)
        (when (and (memq (char-after) '(?. ?! ??))
                   (not (translate--period-after-abbreviation-p)))
          (forward-char)
          (skip-chars-forward " \t\n\"')]}")
          (setq found (point))))
      (or found limit))))

(defun translate--sentence-boundary-forward ()
  "Return the end position of the sentence around point."
  (save-excursion
    (let ((limit (line-end-position))
          found)
      (while (and (not found) (< (point) limit))
        (if (and (memq (char-after) '(?. ?! ??))
                 (not (translate--period-after-abbreviation-p)))
            (progn
              (forward-char)
              (skip-chars-forward "\"')]}")
              (setq found (point)))
          (forward-char)))
      (or found limit))))

(defun translate--normalize-context-sentence (sentence)
  "Normalize whitespace in SENTENCE and return nil when it is empty."
  (let ((trimmed (string-trim
                  (replace-regexp-in-string "[[:space:]\n]+" " " sentence))))
    (unless (string-empty-p trimmed)
      trimmed)))

(defun translate--context-sentence ()
  "Return a fallback sentence around point without external processes."
  (let* ((start (translate--sentence-boundary-backward))
         (end (translate--sentence-boundary-forward))
         (sentence (buffer-substring-no-properties start end)))
    (translate--normalize-context-sentence sentence)))

(defun translate--paragraph-context-source ()
  "Return paragraph text and point offset for external sentence splitting."
  (let* ((bounds (or (bounds-of-thing-at-point 'paragraph)
                     (cons (line-beginning-position) (line-end-position))))
         (start (max (point-min) (car bounds)))
         (end (min (point-max) (cdr bounds))))
    (when (< start end)
      (list :text (buffer-substring-no-properties start end)
            :offset (- (point) start)))))

(defun translate--json-read-sentence-from-buffer ()
  "Read a sentence value from the current buffer as JSON."
  (goto-char (point-min))
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (data (json-read))
         (sentence (cdr (assq 'sentence data))))
    (when (stringp sentence)
      (translate--normalize-context-sentence sentence))))

(defun translate--context-sentence-async (source fallback callback)
  "Resolve a context sentence from SOURCE, then call CALLBACK.

FALLBACK is used when PySBD is not ready, exits with an error, or
times out.  CALLBACK is called exactly once."
  (if (not (and source (translate-pysbd-runtime-files-present-p)))
      (funcall callback fallback)
    (let* ((buffer (generate-new-buffer " *my-translate-pysbd*"))
           (payload (json-encode
                     `((text . ,(plist-get source :text))
                       (offset . ,(plist-get source :offset))
                       (language . ,translate-pysbd-language))))
           (done nil)
           timer
           proc)
      (cl-labels
          ((finish
            (sentence)
            (unless done
              (setq done t)
              (when (timerp timer)
                (cancel-timer timer))
              (when (process-live-p proc)
                (delete-process proc))
              (when (buffer-live-p buffer)
                (kill-buffer buffer))
              (funcall callback (or sentence fallback)))))
        (setq proc
              (make-process
               :name "my-translate-pysbd"
               :buffer buffer
               :command (list (translate-pysbd-python) translate-pysbd-helper-file)
               :connection-type 'pipe
               :noquery t
               :filter
               (lambda (process chunk)
                 (when (buffer-live-p (process-buffer process))
                   (with-current-buffer (process-buffer process)
                     (goto-char (point-max))
                     (insert chunk))))
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (finish
                    (when (and (eq (process-status process) 'exit)
                               (zerop (process-exit-status process))
                               (buffer-live-p (process-buffer process)))
                      (with-current-buffer (process-buffer process)
                        (condition-case nil
                            (translate--json-read-sentence-from-buffer)
                          (error nil)))))))))
        (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
        (setq timer
              (run-at-time translate-pysbd-timeout-seconds nil #'finish nil))
        (process-send-string proc payload)
        (process-send-eof proc)))))

(defun translate--ensure-wordbook-directory ()
  "Create the parent directory of `translate-wordbook-db-file' when needed."
  (let ((dir (file-name-directory (expand-file-name translate-wordbook-db-file))))
    (when dir
      (make-directory dir t))))

(defun translate--with-wordbook-db (fn)
  "Open the wordbook database, run FN with it, then close it."
  (translate--ensure-wordbook-directory)
  (let ((db (sqlite-open (expand-file-name translate-wordbook-db-file))))
    (unwind-protect
        (funcall fn db)
      (sqlite-close db))))

(defun translate--init-wordbook-db (db)
  "Ensure DB has the wordbook schema."
  (sqlite-execute
   db
   "create table if not exists words (
      id integer primary key,
      word text not null,
      normalized_word text not null unique,
      created_at text not null,
      updated_at text not null,
      lookup_count integer not null default 1
    )")
  (sqlite-execute
   db
   "create table if not exists word_occurrences (
      id integer primary key,
      word_id integer not null references words(id) on delete cascade,
      sentence text,
      created_at text not null
    )")
  (sqlite-execute
   db
   "create index if not exists word_occurrences_word_id_idx
      on word_occurrences(word_id)"))

(defun translate--record-word-row (db word normalized now)
  "Record WORD in DB and return (WORD-ID . NEW-P).
NORMALIZED is the lowercase lookup key and NOW is an ISO timestamp."
  (let ((existing-id (caar (sqlite-select
                            db
                            "select id from words where normalized_word = ?"
                            (list normalized)))))
    (if existing-id
        (progn
          (sqlite-execute
           db
           "update words
            set updated_at = ?, lookup_count = lookup_count + 1
            where id = ?"
           (list now existing-id))
          (cons existing-id nil))
      (sqlite-execute
       db
       "insert into words (word, normalized_word, created_at, updated_at, lookup_count)
        values (?, ?, ?, ?, 1)"
       (list word normalized now now))
      (cons (caar (sqlite-select db "select last_insert_rowid()")) t))))

(defun translate--insert-word-occurrence (db word-id sentence now)
  "Insert a context occurrence for WORD-ID into DB."
  (sqlite-execute
   db
   "insert into word_occurrences (word_id, sentence, created_at)
    values (?, ?, ?)"
   (list word-id sentence now)))

(defun translate--insert-word-occurrence-by-id (word-id sentence now)
  "Insert an occurrence for WORD-ID after async sentence resolution."
  (translate--with-wordbook-db
   (lambda (db)
     (translate--init-wordbook-db db)
     (translate--insert-word-occurrence db word-id sentence now))))

(defun translate-record-word-to-wordbook (text)
  "Record TEXT and its current sentence to the reading wordbook database.

Only single-word lookup text is recorded."
  (when (translate--wordbook-eligible-p text)
    (let* ((word (string-trim text))
           (normalized (downcase word))
           (fallback-sentence (translate--context-sentence))
           (source (translate--paragraph-context-source))
           (now (format-time-string "%FT%T%z"))
           word-id
           new-p)
      (translate--with-wordbook-db
       (lambda (db)
         (translate--init-wordbook-db db)
         (pcase-let ((`(,row-word-id . ,row-new-p)
                      (translate--record-word-row db word normalized now)))
           (setq word-id row-word-id
                 new-p row-new-p))))
      (when new-p
        (translate--context-sentence-async
         source
         fallback-sentence
         (lambda (sentence)
           (translate--insert-word-occurrence-by-id word-id sentence now)))))))

(defun translate--sdcv-lookup (text)
  "Run sdcv for TEXT and return parsed JSON entries, or nil on failure."
  (and (executable-find "sdcv")
       (with-temp-buffer
         (when (eq 0 (call-process "sdcv" nil t nil
                                   "--non-interactive"
                                   "--json-output"
                                   "-0"
                                   "-1"
                                   "--data-dir"
                                   translate-stardict-dir
                                   "--only-data-dir"
                                   text))
           (goto-char (point-min))
           (condition-case nil
               (json-read)
             (error nil))))))

(defun translate--exact-word-match-p (entries word)
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

(defun translate--local-dictionary-hit-p (text)
  "Return non-nil when local sdcv dictionaries can explain TEXT."
  (and (translate--wordbook-eligible-p text)
       (translate--exact-word-match-p (translate--sdcv-lookup text) text)))

(defun translate--render ()
  "Build a gt renderer using pop-to-buffer below the selected window."
  (gt-buffer-render
   :name "*gt-reading*"
   :window-config '((display-buffer-below-selected))))

(defun translate--stardict-entry-text (entry dict)
  "Extract definition text from ENTRY for DICT, pretty-printed if available."
  (let* ((raw (cdr (assq 'definition entry)))
         (def (if (and (fboundp 'gt-stardict-pretty-definition) raw)
                  (gt-stardict-pretty-definition (intern dict) raw)
                raw)))
    (when (and (stringp def) (not (string-empty-p def)))
      def)))

(defun translate--clean-stardict-definition (text definition)
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


(defun translate--format-stardict-json (text entries)
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
                                   (let ((cleaned (translate--clean-stardict-definition
                                                   text
                                                   (translate--stardict-entry-text e dict))))
                                     (unless (string-empty-p cleaned)
                                       cleaned)))
                                 (cdr group)))))
        (when defs
          (push (concat dict "\n\n" (string-join defs "\n")) blocks))))
    (string-join (nreverse blocks) "\n\n")))

(defun translate--parse-stardict-task (task)
  "Parse local StarDict TASK results with grouped dictionary formatting."
  (with-slots (text res) task
    (let ((word (car text)))
      (setf res
            (mapcar (lambda (item)
                      (translate--format-stardict-json
                       word
                       (json-read-from-string item)))
                    res)))))

(defun translate--dictionary-translator (text)
  "Build a local dictionary translator for single-word TEXT."
  (gt-translator
    :taker (gt-taker :text text :langs translate-reading-langs :pick nil)
    :engines (gt-stardict-engine :dir translate-stardict-dir
                                 :dir-only t
                                 :parse #'translate--parse-stardict-task)
    :render (translate--render)))

(defun translate--google-translator (text)
  "Build an online Google translator for TEXT."
  (gt-translator
    :taker (gt-taker :text text :langs translate-reading-langs :pick nil)
    :engines (gt-google-engine)
    :render (translate--render)))

(defun translate--choose-translator (text)
  "Pick the appropriate translator for TEXT.
Returns a local StarDict translator when TEXT is a single word that
exists in local dictionaries; otherwise returns a Google translator."
  (if (and (translate--wordbook-eligible-p text)
           (translate--local-dictionary-hit-p text))
      (translate--dictionary-translator text)
    (translate--google-translator text)))

(defun translate-translate-dwim ()
  "Translate active region or word at point.
Prompt when there is no obvious text under point."
  (interactive)
  (require 'gt)
  (let ((text (or (translate--selection-or-word)
                  (read-string "Translate: "))))
    (translate-record-word-to-wordbook text)
    (gt-start (translate--choose-translator text))))

(defun translate--rows-to-wordbook-items (rows group-index)
  "Convert sqlite ROWS to wordbook listing items.
GROUP-INDEX is the row position used to build the group label."
  (mapcar (lambda (row)
            (pcase-let ((`(,word ,count ,created-at ,updated-at ,sentence) row))
              (list :group (substring (nth group-index row) 0 10)
                    :word word
                    :lookup-count count
                    :created-at created-at
                    :updated-at updated-at
                    :sentence sentence)))
          rows))

(defun translate--wordbook-items ()
  "Return all wordbook items sorted by most recent lookup."
  (translate--with-wordbook-db
   (lambda (db)
     (translate--init-wordbook-db db)
     (translate--rows-to-wordbook-items
      (sqlite-select
       db
       "select w.word, w.lookup_count, w.created_at, w.updated_at, o.sentence
        from words w
        left join word_occurrences o on o.word_id = w.id
        order by w.updated_at desc, w.word asc")
      3))))

(defun translate--wordbook-words-on-date (db date)
  "Return words first collected on DATE from DB.
DATE should be formatted as YYYY-MM-DD."
  (sqlite-select
   db
   "select w.word, w.lookup_count, w.created_at, w.updated_at, o.sentence
    from words w
    left join word_occurrences o on o.word_id = w.id
    where substr(w.created_at, 1, 10) = ?
    order by w.created_at desc, w.word asc"
   (list date)))

(defun translate--wordbook-items-on-date (date)
  "Return wordbook items first collected on DATE."
  (translate--with-wordbook-db
   (lambda (db)
     (translate--init-wordbook-db db)
     (translate--rows-to-wordbook-items
      (translate--wordbook-words-on-date db date)
      2))))

(defun translate--wordbook-context-items ()
  "Return wordbook context items sorted by collection time."
  (translate--with-wordbook-db
   (lambda (db)
     (translate--init-wordbook-db db)
     (mapcar (lambda (row)
               (pcase-let ((`(,word ,sentence ,created-at) row))
                 (list :group (substring created-at 0 10)
                       :word word
                       :sentence sentence
                       :created-at created-at)))
             (sqlite-select
              db
              "select w.word, o.sentence, o.created_at
               from word_occurrences o
               join words w on w.id = o.word_id
               order by o.created_at desc, w.word asc")))))

(defun translate--insert-wordbook-item (item)
  "Insert one wordbook ITEM into the current Org listing buffer."
  (insert (format "\n** %s\n" (plist-get item :word)))
  (insert ":PROPERTIES:\n")
  (insert (format ":LOOKUPS: %s\n" (plist-get item :lookup-count)))
  (insert (format ":CREATED_AT: %s\n" (plist-get item :created-at)))
  (insert (format ":UPDATED_AT: %s\n" (plist-get item :updated-at)))
  (insert ":END:\n")
  (when-let* ((sentence (plist-get item :sentence)))
    (insert (format "  %s\n" sentence))))

(defun translate--insert-wordbook-context-item (item)
  "Insert one wordbook context ITEM into the current Org listing buffer."
  (insert (format "\n** %s\n" (plist-get item :word)))
  (insert ":PROPERTIES:\n")
  (insert (format ":CREATED_AT: %s\n" (plist-get item :created-at)))
  (insert ":END:\n")
  (when-let* ((sentence (plist-get item :sentence)))
    (insert (format "  %s\n" sentence))))

(defun translate-list-wordbook ()
  "Show words recorded in the reading wordbook."
  (interactive)
  (org-list-open-buffer "*gt-wordbook*"
                           "Reading Wordbook"
                           #'translate--wordbook-items
                           #'translate--insert-wordbook-item))

(defun translate--show-wordbook-date (date)
  "Show words first collected on DATE."
  (org-list-open-buffer (format "*gt-wordbook-%s*" date)
                           (format "Words Collected on %s" date)
                           (lambda () (translate--wordbook-items-on-date date))
                           #'translate--insert-wordbook-item))

(defun translate-list-wordbook-today ()
  "Show words first collected today."
  (interactive)
  (translate--show-wordbook-date (format-time-string "%F")))

(defun translate-list-wordbook-date (date)
  "Show words first collected on DATE.
DATE should be formatted as YYYY-MM-DD."
  (interactive (list (read-string "Date (YYYY-MM-DD): "
                                  (format-time-string "%F"))))
  (unless (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date)
    (user-error "Date must be formatted as YYYY-MM-DD"))
  (translate--show-wordbook-date date))

(defun translate-list-wordbook-contexts ()
  "Show wordbook records with their captured sentences."
  (interactive)
  (org-list-open-buffer "*gt-wordbook-contexts*"
                           "Reading Wordbook Contexts"
                           #'translate--wordbook-context-items
                           #'translate--insert-wordbook-context-item))

(defun translate-elfeed-dwim ()
  "Translate selected text or word at point while reading Elfeed."
  (interactive)
  (translate-translate-dwim))

(dolist (names '((my/gt-translate-dwim . translate-translate-dwim)
                 (my/gt-list-wordbook . translate-list-wordbook)
                 (my/gt-list-wordbook-today . translate-list-wordbook-today)
                 (my/gt-list-wordbook-date . translate-list-wordbook-date)
                 (my/gt-list-wordbook-contexts . translate-list-wordbook-contexts)
                 (my/gt-pysbd-ensure-installed . translate-pysbd-ensure-installed)
                 (my/elfeed-translate-dwim . translate-elfeed-dwim)))
  (defalias (car names) (cdr names)))

(provide 'translate)
;;; translate.el ends here
