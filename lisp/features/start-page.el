;; -*- lexical-binding: t; -*-
;;; my-start-page.el --- Start page with ASCII art slogan and quick links

;;; Commentary:
;; A start page showing an ASCII art slogan centered in the window,
;; with quick-access links to common directories from
;; `my/common-dirs-alist'.
;;
;; Keys:
;;   n / p     skip to next/previous directory entry
;;   RET / o   open the directory under cursor
;;   g         refresh (re-center after window resize)
;;   q         quit

;;; Code:

(defgroup my-start-page nil
  "Custom start page."
  :group 'convenience)

;; ── Font ─────────────────────────────────────────────────────

(defvar my/start-page--font-block
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

(defcustom my/start-page-slogon-text "SLOW IS FAST"
  "Plain text rendered as ASCII art on the start page.
Characters A-Z and space are supported."
  :type 'string
  :group 'my-start-page)

(defcustom my/start-page-top-padding 3
  "Number of blank lines above the slogan."
  :type 'integer
  :group 'my-start-page)

(defun my/start-page--render-slogon ()
  "Render `my/start-page-slogon-text' as ASCII art using the block font."
  (let* ((chars (mapcar (lambda (c)
                          (or (assoc-default (upcase c) my/start-page--font-block)
                              (assoc-default ?\s my/start-page--font-block)))
                        my/start-page-slogon-text))
         (height (length (cdar my/start-page--font-block)))
         (lines nil))
    (dotimes (i height)
      (push (string-trim-right
             (mapconcat (lambda (rows) (nth i rows)) chars " "))
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

;; ── Faces ─────────────────────────────────────────────────────

(defface my/start-page-dir-name
  '((t :inherit bold))
  "Face for directory entry names on the start page."
  :group 'my-start-page)

(defface my/start-page-slogon
  '((t :inherit (fixed-pitch bold)))
  "Face for the ASCII art slogan on the start page."
  :group 'my-start-page)

;; ── Keymap & Mode ─────────────────────────────────────────────

(defvar my/start-page-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "n") #'my/start-page-next-entry)
    (define-key map (kbd "p") #'my/start-page-prev-entry)
    (define-key map (kbd "RET") #'my/start-page-open-at-point)
    (define-key map (kbd "o") #'my/start-page-open-at-point)
    (define-key map (kbd "g") #'my/start-page-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my/start-page-mode'.")

(defvar-local my/start-page--last-width nil
  "Last window width used to detect resize.")

(define-derived-mode my/start-page-mode special-mode "Start"
  "Major mode for the start page.
\\{my/start-page-mode-map}"
  (buffer-disable-undo)
  (hl-line-mode 1)
  (add-hook 'window-configuration-change-hook
            #'my/start-page--on-window-change))

(defun my/start-page--on-window-change (&rest _)
  "Re-render when window width changes."
  (when (and (derived-mode-p 'my/start-page-mode)
             my/start-page--last-width
             (/= my/start-page--last-width (window-width)))
    (my/start-page-refresh)))

;; ── Layout helpers ────────────────────────────────────────────

(defun my/start-page--center-string (s)
  "Return S centered to current window width."
  (let* ((w (window-width))
         (pad (max 0 (/ (- w (string-width s)) 2))))
    (concat (make-string pad ?\s) s)))

;; ── Directory actions ─────────────────────────────────────────

(defun my/start-page--open-dir (path)
  "Start `find-file' rooted at PATH."
  (let ((default-directory (file-name-as-directory (expand-file-name path))))
    (if (fboundp 'helm-find-files)
        (helm-find-files-1 default-directory)
      (call-interactively #'find-file))))

(defun my/start-page--dir-line-p ()
  "Return non-nil if point is on a directory entry line."
  (not (null (get-text-property (line-beginning-position)
                                'my-start-page-dir))))

(defun my/start-page-next-entry ()
  "Move to the next directory entry."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (my/start-page--dir-line-p)))
      (forward-line 1))
    (unless (my/start-page--dir-line-p)
      (goto-char pos))))

(defun my/start-page-prev-entry ()
  "Move to the previous directory entry."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (my/start-page--dir-line-p)))
      (forward-line -1))
    (unless (my/start-page--dir-line-p)
      (goto-char pos))))

(defun my/start-page-open-at-point ()
  "Open the directory entry on the current line."
  (interactive)
  (let ((path (get-text-property (line-beginning-position)
                                 'my-start-page-dir)))
    (if path
        (my/start-page--open-dir path)
      (user-error "No directory on this line"))))

;; ── Buffer rendering ──────────────────────────────────────────

(defun my/start-page--insert-slogon ()
  "Insert the rendered ASCII art slogan centered."
  (insert (make-string my/start-page-top-padding ?\n))
  (let* ((lines (split-string (my/start-page--render-slogon) "\n"))
         (block-width (apply #'max 0 (mapcar #'string-width lines)))
         (pad (max 0 (/ (- (window-width) block-width) 2))))
    (dolist (line lines)
      (insert (propertize (concat (make-string pad ?\s) line)
                          'face 'my/start-page-slogon)
              "\n"))))

(defun my/start-page--insert-dirs ()
  "Insert quick links from `my/common-dirs-alist'."
  (when (and (boundp 'my/common-dirs-alist) my/common-dirs-alist)
    (let* ((name-width (apply #'max 0
                              (mapcar (lambda (e) (length (car e)))
                                      my/common-dirs-alist)))
           (fmt (format "%%-%ds  →  %%s" name-width))
           (pairs (mapcar (lambda (entry)
                            (cons (format fmt (car entry)
                                          (expand-file-name (cdr entry)))
                                  (expand-file-name (cdr entry))))
                          my/common-dirs-alist))
           (block-width (apply #'max 0
                               (mapcar (lambda (p) (string-width (car p)))
                                       pairs)))
           (pad (max 0 (/ (- (window-width) block-width) 2))))
      (insert "\n\n")
      (dolist (pair pairs)
        (let ((beg (point)))
          (insert (make-string pad ?\s) (car pair) "\n")
          (put-text-property beg (1- (point)) 'my-start-page-dir (cdr pair))
          (put-text-property beg (1- (point)) 'face 'my/start-page-dir-name))))
    (insert "\n")))

(defun my/start-page-refresh ()
  "Refresh the start page buffer (re-center after resize)."
  (interactive)
  (let ((inhibit-read-only t)
        (saved-line (line-number-at-pos)))
    (erase-buffer)
    (my/start-page--insert-slogon)
    (my/start-page--insert-dirs)
    (setq my/start-page--last-width (window-width))
    (goto-char (point-min))
    (forward-line (1- (min saved-line (count-lines (point-min) (point-max)))))
    (unless (my/start-page--dir-line-p)
      (my/start-page-next-entry))))

;;;###autoload
(defun my/start-page ()
  "Show the start page."
  (interactive)
  (let ((buf (get-buffer-create "*start*")))
    (with-current-buffer buf
      (unless (eq major-mode 'my/start-page-mode)
        (my/start-page-mode))
      (my/start-page-refresh))
    (if (called-interactively-p 'any)
        (switch-to-buffer buf)
      buf)))

(provide 'start-page)
;;; my-start-page.el ends here
