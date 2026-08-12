;; -*- lexical-binding: t; -*-
;;; init-completion.el --- Minibuffer completion configuration

;;; Commentary:
;; Vertico/Consult/Orderless and related minibuffer helpers.

;;; Code:

(setq enable-recursive-minibuffers t
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(minibuffer-depth-indicate-mode 1)

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200))

(use-package vertico
  :init
  (vertico-mode 1)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-sort-function #'vertico-sort-history-alpha)
  :custom-face
  (vertico-current ((t (:inherit hl-line
                        :foreground "#fdf4c1"
                        :weight bold
                        :extend t)))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(defun pandaye/completion-consult-git-files (&optional directory)
  "Select a Git-managed file below DIRECTORY with Consult.
Include tracked and untracked non-ignored files.  DIRECTORY defaults to the
current repository root."
  (interactive)
  (require 'consult)
  (require 'vc-git)
  (let* ((root (or directory
                   (vc-git-root default-directory)
                   (user-error "Not inside a Git repository")))
         (default-directory root)
         (files (process-lines "git" "ls-files" "--cached" "--others"
                               "--exclude-standard"))
         (selected (consult--read files
                                  :prompt "Git file: "
                                  :category 'file
                                  :require-match t
                                  :sort nil
                                  :state (consult--file-state))))
    (find-file (expand-file-name selected root))))

;; Preserve the long-standing key target even though recent Consult versions
;; no longer provide this command themselves.
(defalias 'consult-git-files #'pandaye/completion-consult-git-files)

(use-package consult
  :after recentf
  :config
  (consult-customize
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   :preview-key nil)

  (defface pandaye/completion-consult-buffer-annotation
    '((t :inherit font-lock-comment-face :weight normal))
    "Face for custom `consult-buffer' annotations.")

  (defface pandaye/completion-consult-buffer-directory
    '((t :inherit font-lock-comment-face :weight normal))
    "Face for right-aligned buffer directory annotations.")

  (defface pandaye/completion-consult-buffer-virtual-file
    '((t :inherit shadow))
    "Face for unopened file candidates in `pandaye/completion-consult-buffer'.")

  (define-minor-mode pandaye/completion-consult-buffer-annotations-mode
    "Use custom annotations for `pandaye/completion-consult-buffer'."
    :global t
    :init-value t)

  (defvar pandaye/completion-consult-buffer--annotation-width 0
    "Precomputed annotation start column for `pandaye/completion-consult-buffer'.")

  (defvar pandaye/completion-consult-buffer-right-margin 1
    "Columns reserved at the right edge for `pandaye/completion-consult-buffer' annotations.")

  (defun pandaye/completion-consult-buffer--candidate-buffer (candidate)
    "Return buffer represented by Consult CANDIDATE metadata."
    (cond
     ((bufferp candidate) candidate)
     ((stringp candidate) (get-buffer candidate))))

  (defun pandaye/completion-consult-buffer--status (buffer)
    "Return short status string for BUFFER."
    (concat (if (buffer-modified-p buffer) "*" "-")
            (if (buffer-local-value 'buffer-read-only buffer) "%" "-")))

  (defun pandaye/completion-consult-buffer--mode-name (buffer)
    "Return display mode name for BUFFER."
    (with-current-buffer buffer
      (truncate-string-to-width
       (if (stringp mode-name)
           mode-name
         (format-mode-line mode-name))
       18 nil nil "...")))

  (defun pandaye/completion-consult-buffer--truncate-left (text width)
    "Truncate TEXT to WIDTH columns from the left."
    (cond
     ((<= width 0) "")
     ((<= (string-width text) width) text)
     (t
      (let* ((ellipsis "...")
             (ellipsis-width (string-width ellipsis)))
        (if (<= width ellipsis-width)
            (truncate-string-to-width ellipsis width)
          (nreverse
           (truncate-string-to-width (reverse text) width 0 nil ellipsis)))))))

  (defun pandaye/completion-consult-buffer--annotation-start (buffer)
    "Return Consult annotation start column for BUFFER."
    (min (pandaye/completion-consult-buffer--annotation-width-limit)
         (max pandaye/completion-consult-buffer--annotation-width
              (* (ceiling (string-width (buffer-name buffer))
                          consult--annotate-align-step)
                 consult--annotate-align-step))))

  (defun pandaye/completion-consult-buffer--annotation-width-limit ()
    "Return the maximum useful annotation start column."
    (max 0 (- (window-width (minibuffer-window))
              pandaye/completion-consult-buffer-right-margin
              (string-width " --  Lisp Interaction"))))

  (defun pandaye/completion-consult-buffer--source-width (source)
    "Return maximum visible candidate width in Consult SOURCE."
    (let ((width 0))
      (unless (or (plist-get source :hidden)
                  (plist-get source :async))
        (when-let* ((items (plist-get source :items)))
          (dolist (item (ignore-errors
                          (if (functionp items) (funcall items) items)))
            (let ((candidate (or (car-safe item) item)))
              (when (stringp candidate)
                (setq width
                      (max width
                           (string-width
                            (substring-no-properties candidate)))))))))
      width))

  (defun pandaye/completion-consult-buffer--compute-annotation-width (sources)
    "Return the annotation start column for initial Consult SOURCES."
    (let ((width 0))
      (dolist (source sources)
        (setq width
              (max width
                   (pandaye/completion-consult-buffer--source-width
                    (if (symbolp source) (symbol-value source) source)))))
      (min (pandaye/completion-consult-buffer--annotation-width-limit)
           (* (ceiling width consult--annotate-align-step)
              consult--annotate-align-step))))

  (defun pandaye/completion-consult-buffer--align-annotation (orig candidate annotation)
    "Use real spaces for stable `pandaye/completion-consult-buffer' annotations."
    (if (zerop pandaye/completion-consult-buffer--annotation-width)
        (funcall orig candidate annotation)
      (setq consult--annotate-align-width pandaye/completion-consult-buffer--annotation-width)
      (when annotation
        (let* ((candidate (if (fboundp 'consult--tofu-strip)
                              (consult--tofu-strip candidate)
                            (substring-no-properties candidate)))
               (padding (max 1 (- pandaye/completion-consult-buffer--annotation-width
                                   (string-width candidate)))))
          (concat (make-string padding ?\s) annotation)))))

  (defun pandaye/completion-consult-buffer--right-directory (buffer left directory)
    "Return DIRECTORY right-aligned after LEFT for BUFFER."
    (let* ((available (- (window-width (minibuffer-window))
                         (pandaye/completion-consult-buffer--annotation-start buffer)
                         pandaye/completion-consult-buffer-right-margin))
           (directory-width (max 0 (- available (string-width left))))
           (directory (pandaye/completion-consult-buffer--truncate-left directory directory-width))
           (padding (max 0 (- available
                               (string-width left)
                               (string-width directory)))))
      (concat
       (propertize (make-string padding ?\s) 'face 'pandaye/completion-consult-buffer-annotation)
       (propertize directory 'face 'pandaye/completion-consult-buffer-directory))))

  (defun pandaye/completion-consult-buffer-annotate (candidate)
    "Annotate buffer CANDIDATE with status, mode and right-aligned dirname."
    (when-let* ((buffer (pandaye/completion-consult-buffer--candidate-buffer candidate)))
      (let* ((file (buffer-file-name buffer))
             (directory (and file
                             (abbreviate-file-name
                              (file-name-directory file))))
             (left (format " %s  %-18s"
                           (pandaye/completion-consult-buffer--status buffer)
                           (pandaye/completion-consult-buffer--mode-name buffer))))
        (concat
         (propertize left 'face 'pandaye/completion-consult-buffer-annotation)
         (when directory
           (pandaye/completion-consult-buffer--right-directory buffer left directory))))))

  (defun pandaye/completion-consult-buffer--annotated-source (source)
    "Return Consult SOURCE with custom display settings."
    (let ((source (copy-sequence (if (symbolp source) (symbol-value source) source))))
      (setq source (plist-put source :name nil))
      (pcase (plist-get source :category)
        ('buffer
         (setq source (plist-put source :annotate #'pandaye/completion-consult-buffer-annotate)))
        ('file
         (setq source (plist-put source :face 'pandaye/completion-consult-buffer-virtual-file))))
      source))

  (defun pandaye/completion-consult-buffer--sources ()
    "Return `consult-buffer-sources' with custom buffer annotations."
    (if pandaye/completion-consult-buffer-annotations-mode
        (mapcar #'pandaye/completion-consult-buffer--annotated-source consult-buffer-sources)
      consult-buffer-sources))

  (defun pandaye/completion-consult-buffer ()
    "Run `consult-buffer' with custom buffer annotations."
    (interactive)
    (let* ((sources (pandaye/completion-consult-buffer--sources))
           (pandaye/completion-consult-buffer--annotation-width
            (pandaye/completion-consult-buffer--compute-annotation-width sources))
           (consult-preview-key nil))
      (consult-buffer sources)))

  (advice-add 'consult--annotate-align
              :around #'pandaye/completion-consult-buffer--align-annotation))

(provide 'init-completion)
;;; init-completion.el ends here
