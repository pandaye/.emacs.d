;; -*- lexical-binding: t; -*-
;;; org-listing.el --- Shared Org listing helpers

;;; Commentary:
;; Shared helpers for generated Org buffers with grouped, paginated listings.

;;; Code:

(require 'button)
(require 'org)
(require 'seq)

(defgroup org-listing nil
  "Shared helpers for generated Org listing buffers."
  :group 'org)

(defvaralias 'my/org-list-page-size 'org-list-page-size)

(defcustom org-list-page-size 20
  "Default number of items shown per page in generated Org lists."
  :type 'integer
  :group 'org-listing)

(defvar-local org-list--title nil
  "Current listing title for the active Org list buffer.")

(defvar-local org-list--page 0
  "Zero-based current page index for the active Org list buffer.")

(defvar-local org-list--page-size org-list-page-size
  "Page size for the active Org list buffer.")

(defvar-local org-list--items-function nil
  "Function returning listing items for the active Org list buffer.")

(defvar-local org-list--insert-item-function nil
  "Function inserting one listing item into the active Org list buffer.")

(defun org-list-group-label (time)
  "Return year-month group label for TIME."
  (format-time-string "%Y-%m" time))

(defun org-list-make-link (file description)
  "Return an Org file link to FILE with DESCRIPTION."
  (org-link-make-string (concat "file:" file) description))

(defun org-list--total-pages (item-count page-size)
  "Return total page count for ITEM-COUNT and PAGE-SIZE."
  (max 1 (ceiling (/ (float (max item-count 1)) page-size))))

(defun org-list--current-slice (items page page-size)
  "Return ITEMS slice for PAGE and PAGE-SIZE."
  (let* ((start (* page page-size))
         (end (min (length items) (+ start page-size))))
    (seq-subseq items start end)))

(defun org-list--line-link-context ()
  "Return Org link context for the first link on current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward org-link-any-re (line-end-position) t)
      (goto-char (match-beginning 0))
      (org-element-context))))

(defun org-list-open-at-point ()
  "Open button or Org link at point."
  (interactive)
  (let ((button (button-at (point))))
    (cond
     (button (push-button button))
     ((eq (org-element-type (org-element-context)) 'link)
      (org-open-at-point))
     ((org-list--line-link-context)
      (save-excursion
        (beginning-of-line)
        (re-search-forward org-link-any-re (line-end-position) t)
        (goto-char (match-beginning 0))
        (org-open-at-point)))
     (t (user-error "No link found on current line")))))

(defun org-list-copy-link-at-point ()
  "Copy Org link target at point.
Prefer the file link under point, or the current line's first Org link." 
  (interactive)
  (save-excursion
    (let ((context (org-element-context))
          copied)
      (unless (eq (org-element-type context) 'link)
        (setq context (org-list--line-link-context)))
      (when (eq (org-element-type context) 'link)
        (setq copied (org-element-property :raw-link context)))
      (unless copied
        (user-error "No Org link found on current line"))
      (kill-new copied)
      (message "Copied link: %s" copied))))

(defun org-list--insert-button (label action enabled)
  "Insert navigation button LABEL calling ACTION when ENABLED."
  (if enabled
      (insert-text-button label
                          'action (lambda (_button) (funcall action))
                          'follow-link t)
    (insert (propertize label 'face 'shadow))))

(defun org-list--insert-navigation (item-count total-pages &optional include-title)
  "Insert navigation section for ITEM-COUNT and TOTAL-PAGES.
When INCLUDE-TITLE is non-nil, insert the buffer title first."
  (when include-title
    (insert (format "#+title: %s\n\n" org-list--title)))
  (insert (format "Page %d/%d · %d items · %d per page\n"
                  (1+ org-list--page)
                  total-pages
                  item-count
                  org-list--page-size))
  (org-list--insert-button "[Prev]" #'org-list-previous-page (> org-list--page 0))
  (insert " ")
  (org-list--insert-button "[Next]" #'org-list-next-page (< (1+ org-list--page) total-pages))
  (insert " ")
  (org-list--insert-button "[Refresh]" #'org-list-refresh t)
  (insert "\n\n"))

(defun org-list--insert-grouped-items (items)
  "Insert ITEMS grouped by their :group label."
  (let ((current-group nil))
    (dolist (item items)
      (let ((group (plist-get item :group)))
        (unless (equal group current-group)
          (when current-group
            (insert "\n"))
          (insert (format "\n* %s\n" group))
          (setq current-group group))
        (funcall org-list--insert-item-function item)))))

(defun org-list-refresh ()
  "Refresh the current generated Org list buffer."
  (interactive)
  (unless (and org-list--items-function org-list--insert-item-function)
    (user-error "Current buffer is not a managed Org list"))
  (let* ((items (funcall org-list--items-function))
         (item-count (length items))
         (page-size (max 1 org-list--page-size))
         (total-pages (org-list--total-pages item-count page-size))
         (page (min org-list--page (1- total-pages)))
         (page-items (org-list--current-slice items page page-size))
         (inhibit-read-only t))
    (setq-local org-list--page page)
    (erase-buffer)
    (org-list--insert-navigation item-count total-pages t)
    (if page-items
        (org-list--insert-grouped-items page-items)
      (insert "_No entries found._\n"))
    (insert "\n")
    (org-list--insert-navigation item-count total-pages)
    (goto-char (point-min))))

(defun org-list-next-page ()
  "Move to the next page in the current Org list buffer."
  (interactive)
  (setq-local org-list--page (1+ org-list--page))
  (org-list-refresh))

(defun org-list-previous-page ()
  "Move to the previous page in the current Org list buffer."
  (interactive)
  (setq-local org-list--page (max 0 (1- org-list--page)))
  (org-list-refresh))

(defvar org-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "[") #'org-list-previous-page)
    (define-key map (kbd "]") #'org-list-next-page)
    (define-key map (kbd "r") #'org-list-refresh)
    (define-key map (kbd "c") #'org-list-copy-link-at-point)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'org-list-open-at-point)
    map)
  "Keymap for `org-list-mode'.")

(define-derived-mode org-list-mode org-mode "My-Org-List"
  "Major mode for generated grouped and paginated Org listing buffers."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t))

(defun org-list-open-buffer (buffer-name title items-function insert-item-function
                                            &optional page-size)
  "Open BUFFER-NAME with TITLE using ITEMS-FUNCTION and INSERT-ITEM-FUNCTION.
Optional PAGE-SIZE overrides `org-list-page-size'."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (org-list-mode)
      (setq-local org-list--title title)
      (setq-local org-list--page 0)
      (setq-local org-list--page-size (or page-size org-list-page-size))
      (setq-local org-list--items-function items-function)
      (setq-local org-list--insert-item-function insert-item-function)
      (org-list-refresh))
    (pop-to-buffer buffer)))

(dolist (names '((my/org-list-group-label . org-list-group-label)
                 (my/org-list-make-link . org-list-make-link)
                 (my/org-list-open-buffer . org-list-open-buffer)
                 (my/org-list-refresh . org-list-refresh)
                 (my/org-list-next-page . org-list-next-page)
                 (my/org-list-previous-page . org-list-previous-page)))
  (defalias (car names) (cdr names)))

(provide 'org-listing)
;;; org-listing.el ends here
