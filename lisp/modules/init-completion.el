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
                        :extend t))))
  :bind
  ("C-c C-r" . vertico-repeat))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after recentf
  :bind
  (("C-c b b" . my/consult-buffer)
   ("C-x B" . consult-buffer-other-window)
   ("C-c f g" . consult-git-files)
   ("C-c f G" . consult-git-grep)
   ("C-c f f" . consult-find)
   ("C-s" . consult-line)
   ("C-r" . consult-line)
   ("M-y" . consult-yank-pop))
  :config
  (consult-customize
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   :preview-key nil)

  (defface my/consult-buffer-annotation
    '((t :inherit font-lock-comment-face :weight normal))
    "Face for custom `consult-buffer' annotations.")

  (defface my/consult-buffer-directory
    '((t :inherit font-lock-comment-face :weight normal))
    "Face for right-aligned buffer directory annotations.")

  (defface my/consult-buffer-virtual-file
    '((t :inherit shadow))
    "Face for unopened file candidates in `my/consult-buffer'.")

  (define-minor-mode my/consult-buffer-annotations-mode
    "Use custom annotations for `my/consult-buffer'."
    :global t
    :init-value t)

  (defvar my/consult-buffer--annotation-width 0
    "Precomputed annotation start column for `my/consult-buffer'.")

  (defvar my/consult-buffer-right-margin 1
    "Columns reserved at the right edge for `my/consult-buffer' annotations.")

  (defun my/consult-buffer--candidate-buffer (candidate)
    "Return buffer represented by Consult CANDIDATE metadata."
    (cond
     ((bufferp candidate) candidate)
     ((stringp candidate) (get-buffer candidate))))

  (defun my/consult-buffer--status (buffer)
    "Return short status string for BUFFER."
    (concat (if (buffer-modified-p buffer) "*" "-")
            (if (buffer-local-value 'buffer-read-only buffer) "%" "-")))

  (defun my/consult-buffer--mode-name (buffer)
    "Return display mode name for BUFFER."
    (with-current-buffer buffer
      (truncate-string-to-width
       (if (stringp mode-name)
           mode-name
         (format-mode-line mode-name))
       18 nil nil "...")))

  (defun my/consult-buffer--truncate-left (text width)
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

  (defun my/consult-buffer--annotation-start (buffer)
    "Return Consult annotation start column for BUFFER."
    (min (my/consult-buffer--annotation-width-limit)
         (max my/consult-buffer--annotation-width
              (* (ceiling (string-width (buffer-name buffer))
                          consult--annotate-align-step)
                 consult--annotate-align-step))))

  (defun my/consult-buffer--annotation-width-limit ()
    "Return the maximum useful annotation start column."
    (max 0 (- (window-width (minibuffer-window))
              my/consult-buffer-right-margin
              (string-width " --  Lisp Interaction"))))

  (defun my/consult-buffer--source-width (source)
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

  (defun my/consult-buffer--compute-annotation-width (sources)
    "Return the annotation start column for initial Consult SOURCES."
    (let ((width 0))
      (dolist (source sources)
        (setq width
              (max width
                   (my/consult-buffer--source-width
                    (if (symbolp source) (symbol-value source) source)))))
      (min (my/consult-buffer--annotation-width-limit)
           (* (ceiling width consult--annotate-align-step)
              consult--annotate-align-step))))

  (defun my/consult-buffer--align-annotation (orig candidate annotation)
    "Use real spaces for stable `my/consult-buffer' annotations."
    (if (zerop my/consult-buffer--annotation-width)
        (funcall orig candidate annotation)
      (setq consult--annotate-align-width my/consult-buffer--annotation-width)
      (when annotation
        (let* ((candidate (if (fboundp 'consult--tofu-strip)
                              (consult--tofu-strip candidate)
                            (substring-no-properties candidate)))
               (padding (max 1 (- my/consult-buffer--annotation-width
                                   (string-width candidate)))))
          (concat (make-string padding ?\s) annotation)))))

  (defun my/consult-buffer--right-directory (buffer left directory)
    "Return DIRECTORY right-aligned after LEFT for BUFFER."
    (let* ((available (- (window-width (minibuffer-window))
                         (my/consult-buffer--annotation-start buffer)
                         my/consult-buffer-right-margin))
           (directory-width (max 0 (- available (string-width left))))
           (directory (my/consult-buffer--truncate-left directory directory-width))
           (padding (max 0 (- available
                               (string-width left)
                               (string-width directory)))))
      (concat
       (propertize (make-string padding ?\s) 'face 'my/consult-buffer-annotation)
       (propertize directory 'face 'my/consult-buffer-directory))))

  (defun my/consult-buffer-annotate (candidate)
    "Annotate buffer CANDIDATE with status, mode and right-aligned dirname."
    (when-let* ((buffer (my/consult-buffer--candidate-buffer candidate)))
      (let* ((file (buffer-file-name buffer))
             (directory (and file
                             (abbreviate-file-name
                              (file-name-directory file))))
             (left (format " %s  %-18s"
                           (my/consult-buffer--status buffer)
                           (my/consult-buffer--mode-name buffer))))
        (concat
         (propertize left 'face 'my/consult-buffer-annotation)
         (when directory
           (my/consult-buffer--right-directory buffer left directory))))))

  (defun my/consult-buffer--annotated-source (source)
    "Return Consult SOURCE with custom display settings."
    (let ((source (copy-sequence (if (symbolp source) (symbol-value source) source))))
      (setq source (plist-put source :name nil))
      (pcase (plist-get source :category)
        ('buffer
         (setq source (plist-put source :annotate #'my/consult-buffer-annotate)))
        ('file
         (setq source (plist-put source :face 'my/consult-buffer-virtual-file))))
      source))

  (defun my/consult-buffer--sources ()
    "Return `consult-buffer-sources' with custom buffer annotations."
    (if my/consult-buffer-annotations-mode
        (mapcar #'my/consult-buffer--annotated-source consult-buffer-sources)
      consult-buffer-sources))

  (defun my/consult-buffer ()
    "Run `consult-buffer' with custom buffer annotations."
    (interactive)
    (let* ((sources (my/consult-buffer--sources))
           (my/consult-buffer--annotation-width
            (my/consult-buffer--compute-annotation-width sources))
           (consult-preview-key nil))
      (consult-buffer sources)))

  (advice-add 'consult--annotate-align
              :around #'my/consult-buffer--align-annotation))

(provide 'init-completion)
;;; init-completion.el ends here
