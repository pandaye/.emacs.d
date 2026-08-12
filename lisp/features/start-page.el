;; -*- lexical-binding: t; -*-
;;; start-page.el --- Start page with ASCII art slogan and quick links

;;; Commentary:
;; A start page showing an ASCII art slogan centered in the window,
;; with quick-access links to common directories from
;; `common-dirs-alist'.
;;
;; Keys:
;;   n / p     skip to next/previous directory entry
;;   RET / o   open the directory under cursor
;;   g         refresh (re-center after window resize)
;;   q         quit

;;; Code:

(defvaralias 'my/start-page-slogon-text 'start-page-slogon-text)
(defvaralias 'my/start-page-top-padding 'start-page-top-padding)

(defgroup start-page nil
  "Custom start page."
  :group 'convenience)

;; ── Font ─────────────────────────────────────────────────────

(defvar start-page--font-block
  '((?\s . ("       " "       " "       " "       " "       " "       "))
    (?. . ("   " "   " "   " "   " " _ " "(_)"))
    (?A . ("    _    " "   / \\   " "  / _ \\  " " / ___ \\ " "/_/   \\_\\" "         "))
    (?B . (" ____  " "| __ ) " "|  _ \\ " "| |_) |" "|____/ " "       "))
    (?C . ("  ____ " " / ___|" "| |    " "| |___ " " \\____|" "       "))
    (?D . (" ____  " "|  _ \\ " "| | | |" "| |_| |" "|____/ " "       "))
    (?E . (" _____ " "| ____|" "|  _|  " "| |___ " "|_____|" "       "))
    (?F . (" _____ " "|  ___|" "| |_   " "|  _|  " "|_|    " "       "))
    (?G . ("  ____ " " / ___|" "| |  _ " "| |_| |" " \\____|" "       "))
    (?H . (" _   _ " "| | | |" "| |_| |" "|  _  |" "|_| |_|" "       "))
    (?I . (" ___ " "|_ _|" " | | " " | | " "|___|" "     "))
    (?J . ("     _ " "    | |" " _  | |" "| |_| |" " \\___/ " "       "))
    (?K . (" _  __" "| |/ /" "| ' / " "| . \\ " "|_|\\_\\" "      "))
    (?L . (" _     " "| |    " "| |    " "| |___ " "|_____|" "       "))
    (?M . (" __  __ " "|  \\/  |" "| |\\/| |" "| |  | |" "|_|  |_|" "        "))
    (?N . (" _   _ " "| \\ | |" "|  \\| |" "| |\\  |" "|_| \\_|" "       "))
    (?O . ("  ___  " " / _ \\ " "| | | |" "| |_| |" " \\___/ " "       "))
    (?P . (" ____  " "|  _ \\ " "| |_) |" "|  __/ " "|_|    " "       "))
    (?Q . ("  ___  " " / _ \\ " "| | | |" "| |_| |" " \\__\\_\\" "       "))
    (?R . (" ____  " "|  _ \\ " "| |_) |" "|  _ < " "|_| \\_\\" "       "))
    (?S . (" ____  " "/ ___| " "\\___ \\ " " ___) |" "|____/ " "       "))
    (?T . (" _____ " "|_   _|" "  | |  " "  | |  " "  |_|  " "       "))
    (?U . (" _   _ " "| | | |" "| | | |" "| |_| |" " \\___/ " "       "))
    (?V . ("__     __" "\\ \\   / /" " \\ \\ / / " "  \\ V /  " "   \\_/   " "         "))
    (?W . ("__        __" "\\ \\      / /" " \\ \\ /\\ / / " "  \\ V  V /  " "   \\_/\\_/   " "            "))
    (?X . ("__  __" "\\ \\/ /" " \\  / " " /  \\ " "/_/\\_\\" "      "))
    (?Y . ("__   __" "\\ \\ / /" " \\ V / " "  | |  " "  |_|  " "       "))
    (?Z . (" _____" "|__  /" "  / / " " / /_ " "/____|" "      ")))
  "Pure ASCII FIGlet-style font for slogan rendering.
Uses only ASCII characters to avoid terminal/font width issues.")

;; ── Slogon ────────────────────────────────────────────────────

(defcustom start-page-slogon-text "SLOW IS FAST"
  "Plain text rendered as ASCII art on the start page.
Characters A-Z and space are supported."
  :type 'string
  :group 'start-page)

(defcustom start-page-top-padding 3
  "Number of blank lines above the slogan."
  :type 'integer
  :group 'start-page)

(defun start-page--render-slogon ()
  "Render `start-page-slogon-text' as ASCII art using the block font."
  (let* ((chars (mapcar (lambda (c)
                          (or (assoc-default (upcase c) start-page--font-block)
                              (assoc-default ?\s start-page--font-block)))
                        start-page-slogon-text))
         (height (length (cdar start-page--font-block)))
         (lines nil))
    (dotimes (i height)
      (push (string-trim-right
             (mapconcat (lambda (rows) (nth i rows)) chars " "))
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

;; ── Faces ─────────────────────────────────────────────────────

(defface start-page-dir-name
  '((t :inherit bold))
  "Face for directory entry names on the start page."
  :group 'start-page)

(defface start-page-slogon
  '((t :inherit (fixed-pitch bold)))
  "Face for the ASCII art slogan on the start page."
  :group 'start-page)

;; ── Keymap & Mode ─────────────────────────────────────────────

(defvar start-page-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "n") #'start-page-next-entry)
    (define-key map (kbd "p") #'start-page-prev-entry)
    (define-key map (kbd "RET") #'start-page-open-at-point)
    (define-key map (kbd "o") #'start-page-open-at-point)
    (define-key map (kbd "g") #'start-page-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `start-page-mode'.")

(defvar-local start-page--last-width nil
  "Last window width used to detect resize.")

(define-derived-mode start-page-mode special-mode "Start"
  "Major mode for the start page.
\\{start-page-mode-map}"
  (buffer-disable-undo)
  (hl-line-mode 1)
  (add-hook 'window-configuration-change-hook
            #'start-page--on-window-change))

(defun start-page--on-window-change (&rest _)
  "Re-render when window width changes."
  (when (and (derived-mode-p 'start-page-mode)
             start-page--last-width
             (/= start-page--last-width (window-width)))
    (start-page-refresh)))

;; ── Layout helpers ────────────────────────────────────────────

(defun start-page--center-string (s)
  "Return S centered to current window width."
  (let* ((w (window-width))
         (pad (max 0 (/ (- w (string-width s)) 2))))
    (concat (make-string pad ?\s) s)))

;; ── Directory actions ─────────────────────────────────────────

(defun start-page--open-dir (path)
  "Start `find-file' rooted at PATH."
  (let ((default-directory (file-name-as-directory (expand-file-name path))))
    (if (fboundp 'helm-find-files)
        (helm-find-files-1 default-directory)
      (call-interactively #'find-file))))

(defun start-page--dir-line-p ()
  "Return non-nil if point is on a directory entry line."
  (not (null (get-text-property (line-beginning-position)
                                'start-page-dir))))

(defun start-page-next-entry ()
  "Move to the next directory entry."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (start-page--dir-line-p)))
      (forward-line 1))
    (unless (start-page--dir-line-p)
      (goto-char pos))))

(defun start-page-prev-entry ()
  "Move to the previous directory entry."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (start-page--dir-line-p)))
      (forward-line -1))
    (unless (start-page--dir-line-p)
      (goto-char pos))))

(defun start-page-open-at-point ()
  "Open the directory entry on the current line."
  (interactive)
  (let ((path (get-text-property (line-beginning-position)
                                 'start-page-dir)))
    (if path
        (start-page--open-dir path)
      (user-error "No directory on this line"))))

;; ── Buffer rendering ──────────────────────────────────────────

(defun start-page--insert-slogon ()
  "Insert the rendered ASCII art slogan centered."
  (insert (make-string start-page-top-padding ?\n))
  (let* ((lines (split-string (start-page--render-slogon) "\n"))
         (block-width (apply #'max 0 (mapcar #'string-width lines)))
         (pad (max 0 (/ (- (window-width) block-width) 2))))
    (dolist (line lines)
      (insert (propertize (concat (make-string pad ?\s) line)
                          'face 'start-page-slogon)
              "\n"))))

(defun start-page--insert-dirs ()
  "Insert quick links from `common-dirs-alist'."
  (when (and (boundp 'common-dirs-alist) common-dirs-alist)
    (let* ((name-width (apply #'max 0
                              (mapcar (lambda (e) (length (car e)))
                                      common-dirs-alist)))
           (fmt (format "%%-%ds  →  %%s" name-width))
           (pairs (mapcar (lambda (entry)
                            (cons (format fmt (car entry)
                                          (expand-file-name (cdr entry)))
                                  (expand-file-name (cdr entry))))
                          common-dirs-alist))
           (block-width (apply #'max 0
                               (mapcar (lambda (p) (string-width (car p)))
                                       pairs)))
           (pad (max 0 (/ (- (window-width) block-width) 2))))
      (insert "\n\n")
      (dolist (pair pairs)
        (let ((beg (point)))
          (insert (make-string pad ?\s) (car pair) "\n")
          (put-text-property beg (1- (point)) 'start-page-dir (cdr pair))
          (put-text-property beg (1- (point)) 'face 'start-page-dir-name))))
    (insert "\n")))

(defun start-page-refresh ()
  "Refresh the start page buffer (re-center after resize)."
  (interactive)
  (let ((inhibit-read-only t)
        (saved-line (line-number-at-pos)))
    (erase-buffer)
    (start-page--insert-slogon)
    (start-page--insert-dirs)
    (setq start-page--last-width (window-width))
    (goto-char (point-min))
    (forward-line (1- (min saved-line (count-lines (point-min) (point-max)))))
    (unless (start-page--dir-line-p)
      (start-page-next-entry))))

;;;###autoload
(defun start-page ()
  "Show the start page."
  (interactive)
  (let ((buf (get-buffer-create "*start*")))
    (with-current-buffer buf
      (unless (eq major-mode 'start-page-mode)
        (start-page-mode))
      (start-page-refresh))
    (if (called-interactively-p 'any)
        (switch-to-buffer buf)
      buf)))

(dolist (names '((my/start-page . start-page)
                 (my/start-page-refresh . start-page-refresh)
                 (my/start-page-next-entry . start-page-next-entry)
                 (my/start-page-prev-entry . start-page-prev-entry)
                 (my/start-page-open-at-point . start-page-open-at-point)))
  (defalias (car names) (cdr names)))

(provide 'start-page)
;;; start-page.el ends here
