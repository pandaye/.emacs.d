;;; my-static-blog.el --- Simple Org static blog publishing -*- lexical-binding: t; -*-

;;; Commentary:
;; Publish a small, fast static blog from Org files using built-in Org exporters.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'ox-html)

(defgroup my-static-blog nil
  "Static blog publishing for Org files."
  :group 'org)

(defcustom my-static-blog-title "Lantern Index"
  "Site title used by the blog template."
  :type 'string)

(defcustom my-static-blog-subtitle "Notes gathered where the quiet light lands"
  "Subtitle shown on the blog index page."
  :type 'string)

(defcustom my-static-blog-public-directory
  (expand-file-name "public" user-emacs-directory)
  "Directory where the static blog is published."
  :type 'directory)

(defcustom my-static-blog-assets-directory
  (expand-file-name "assets" user-emacs-directory)
  "Directory containing static blog assets."
  :type 'directory)

(defun my-static-blog--format-date (date)
  "Return a display string for DATE."
  (cond
   ((null date) "")
   ((string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" date) (match-string 0 date))
   (t date)))

(defun my-static-blog--read-keyword (keyword)
  "Read Org KEYWORD from current buffer."
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (node)
      (when (string= (org-element-property :key node) keyword)
        (org-element-property :value node)))
    nil t))

(defun my-static-blog--read-org-post (file)
  "Read post metadata from Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((title (or (my-static-blog--read-keyword "TITLE")
                      (file-name-base file)))
           (date (or (my-static-blog--read-keyword "DATE")
                     (format-time-string "%Y-%m-%d" (file-attribute-modification-time
                                                      (file-attributes file)))))
           (description (my-static-blog--read-keyword "DESCRIPTION"))
           (slug (file-name-base file)))
      (list :file file
            :slug slug
            :url (concat slug ".html")
            :title title
            :date (my-static-blog--format-date date)
            :description description))))

(defun my-static-blog--read-html-meta (file name)
  "Read meta NAME content from HTML FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward
           (format "<meta name=\"%s\" content=\"\\([^\"]*\\)\">" (regexp-quote name))
           nil t)
      (match-string 1))))

(defun my-static-blog--read-html-title (file)
  "Read title text from HTML FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "<title>\\([^<]+\\)</title>" nil t)
        (match-string 1)
      (file-name-base file))))

(defun my-static-blog--read-published-post (file)
  "Read post metadata from generated HTML FILE."
  (list :file file
        :slug (file-name-base file)
        :url (file-name-nondirectory file)
        :title (or (my-static-blog--read-html-meta file "blog-title")
                   (my-static-blog--read-html-title file))
        :date (my-static-blog--format-date (or (my-static-blog--read-html-meta file "blog-date") ""))
        :description (my-static-blog--read-html-meta file "description")))

(defun my-static-blog--collect-posts ()
  "Collect generated blog post metadata sorted by date descending."
  (let ((files (when (file-directory-p my-static-blog-public-directory)
                 (directory-files my-static-blog-public-directory t "\\.html\\'"))))
    (sort (mapcar #'my-static-blog--read-published-post
                  (cl-remove-if (lambda (file)
                                  (string= "index.html" (file-name-nondirectory file)))
                                files))
          (lambda (a b)
            (string> (or (plist-get a :date) "")
                     (or (plist-get b :date) ""))))))

(defun my-static-blog--html-escape (text)
  "Escape TEXT for HTML."
  (replace-regexp-in-string
   "'" "&#39;"
   (replace-regexp-in-string
    "\"" "&quot;"
    (replace-regexp-in-string
     ">" "&gt;"
     (replace-regexp-in-string
      "<" "&lt;"
      (replace-regexp-in-string "&" "&amp;" (or text "") t t)
      t t)
     t t)
    t t)
   t t))

(defun my-static-blog--site-header ()
  "Return shared site header HTML."
  (format "<header class=\"site-header\"><a class=\"site-title\" href=\"index.html\">%s</a><nav class=\"site-nav\"><a href=\"index.html\">首页</a><a href=\"index.html\">归档</a><a href=\"#\">关于</a></nav></header>"
          (my-static-blog--html-escape my-static-blog-title)))

(defun my-static-blog--site-footer ()
  "Return shared site footer HTML."
  "<footer class=\"site-footer\">由 Org mode 生成。让页面退后，让文字向前。</footer>")

(defun my-static-blog--render-head (title &optional meta script)
  "Return document head HTML for TITLE, optional META and SCRIPT."
  (concat "<head>\n"
          "<meta charset=\"utf-8\">\n"
          "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
          meta
          "<title>" (my-static-blog--html-escape title) "</title>\n"
          "<link rel=\"stylesheet\" href=\"assets/blog.css\">\n"
          script
          "</head>\n"))

(defun my-static-blog--render-document (title body &optional meta script)
  "Return a complete HTML document with TITLE and BODY."
  (concat "<!doctype html>\n<html lang=\"zh-CN\">\n"
          (my-static-blog--render-head title meta script)
          "<body>\n"
          body
          "\n</body>\n</html>\n"))

(defun my-static-blog--render-page (contents post)
  "Wrap exported CONTENTS using POST metadata."
  (let* ((title (plist-get post :title))
         (date (plist-get post :date))
         (description (plist-get post :description))
         (body (concat (my-static-blog--site-header)
                       "<main class=\"post-shell\"><article class=\"page post\">"
                       "<header class=\"post-header\"><h1 class=\"post-title\">"
                       (my-static-blog--html-escape title)
                       "</h1>"
                       (when (not (string-empty-p date))
                         (format "<div class=\"post-meta\">%s</div>" (my-static-blog--html-escape date)))
                       (when (and description (not (string-empty-p description)))
                         (format "<p class=\"post-description\">%s</p>" (my-static-blog--html-escape description)))
                       "</header><div class=\"post-content\">"
                       contents
                       "</div></article></main>"
                       (my-static-blog--site-footer)))
         (meta (concat "<meta name=\"blog-title\" content=\"" (my-static-blog--html-escape title) "\">\n"
                       "<meta name=\"blog-date\" content=\"" (my-static-blog--html-escape date) "\">\n"
                       (when (and description (not (string-empty-p description)))
                         (format "<meta name=\"description\" content=\"%s\">\n"
                                 (my-static-blog--html-escape description))))))
    (my-static-blog--render-document
     title
     body
     meta
     "<script src=\"assets/blog.js\" defer></script>\n")))

(defun my-static-blog--cjk-char-p (char)
  "Return non-nil when CHAR is a CJK character."
  (or (and (>= char #x3400) (<= char #x9fff))
      (and (>= char #xf900) (<= char #xfaff))
      (and (>= char #x20000) (<= char #x2ffff))))

(defun my-static-blog--remove-cjk-soft-break-spaces (html)
  "Remove spaces inserted by Org soft line breaks between CJK text in HTML."
  (let ((start 0))
    (with-temp-buffer
      (while (string-match "<[^>]+>" html start)
        (my-static-blog--insert-cjk-cleaned-text (substring html start (match-beginning 0)))
        (insert (match-string 0 html))
        (setq start (match-end 0)))
      (my-static-blog--insert-cjk-cleaned-text (substring html start))
      (buffer-string))))

(defun my-static-blog--insert-cjk-cleaned-text (text)
  "Insert TEXT with CJK-to-CJK whitespace removed."
  (let ((index 0)
        (length (length text)))
    (while (< index length)
      (if (and (> index 0)
               (< (1+ index) length)
               (memq (aref text index) '(?\s ?\n ?\t))
               (my-static-blog--cjk-char-p (aref text (1- index)))
               (my-static-blog--cjk-char-p (aref text (1+ index))))
          (setq index (1+ index))
        (insert (aref text index))
        (setq index (1+ index))))))

(defun my-static-blog--export-options ()
  "Return Org HTML export options for one blog post."
  `(:html-doctype "html5"
    :html-html5-fancy t
    :html-head ""
    :html-head-include-default-style nil
    :html-head-include-scripts nil
    :html-preamble nil
    :html-postamble nil
    :htmlized-source nil
    :with-author nil
    :with-creator nil
    :with-date nil
    :with-email nil
    :with-toc nil
    :section-numbers nil
    :html-wrap-src-lines t
    :html-container "div"
    :html-divs ((preamble "div" "preamble")
                (content "div" "content")
                (postamble "div" "postamble"))
    :html-link-home "index.html"
    :html-link-up "index.html"
    :html-home/up-format ""))

(defun my-static-blog--htmlize-output-type ()
  "Return Org HTML code highlighting strategy for blog export."
  (if (require 'htmlize nil t) 'css nil))

(defun my-static-blog--id-target-file (id)
  "Return the Org file containing ID, or nil."
  (when (and id (not (string-empty-p id)))
    (or (org-id-find-id-file id)
        (when (fboundp 'org-roam-node-from-id)
          (let ((node (ignore-errors (org-roam-node-from-id id))))
            (when (and node (fboundp 'org-roam-node-file))
              (org-roam-node-file node))))
        (let ((marker (org-id-find id 'marker)))
          (when marker
            (prog1 (buffer-file-name (marker-buffer marker))
              (move-marker marker nil)))))))

(defun my-static-blog--html-file-name-for-org (file)
  "Return blog HTML file name for Org FILE."
  (concat (file-name-base file) ".html"))

(defun my-static-blog--id-link-replacement (id description)
  "Return replacement Org link for ID with DESCRIPTION."
  (let* ((target-file (my-static-blog--id-target-file id))
         (label (or description id)))
    (if target-file
        (let ((path (format "file:%s::#ID-%s"
                            (my-static-blog--html-file-name-for-org target-file) id)))
          (if description
              (format "[[%s][%s]]" path description)
            (format "[[%s]]" path)))
      (format "@@html:<a href=\"#ID-%s\">%s</a>@@"
              (my-static-blog--html-escape id)
              (my-static-blog--html-escape label)))))

(defun my-static-blog--rewrite-id-links-for-export ()
  "Rewrite Org id links to static HTML links before export."
  (let (links)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "id")
          (push link links))))
    (dolist (link (sort links (lambda (a b)
                               (> (org-element-property :begin a)
                                  (org-element-property :begin b)))))
      (let* ((begin (org-element-property :begin link))
             (end (org-element-property :end link))
             (id (org-element-property :path link))
             (description (when (org-element-contents link)
                            (string-trim
                             (buffer-substring-no-properties
                              (org-element-property :contents-begin link)
                              (org-element-property :contents-end link))))))
        (delete-region begin end)
        (goto-char begin)
        (insert (my-static-blog--id-link-replacement id description))))))

(defun my-static-blog--copy-assets ()
  "Copy blog assets into the public directory."
  (let ((target (expand-file-name "assets" my-static-blog-public-directory)))
    (make-directory target t)
    (copy-file (expand-file-name "blog.css" my-static-blog-assets-directory)
                (expand-file-name "blog.css" target)
                t)
    (copy-file (expand-file-name "blog.js" my-static-blog-assets-directory)
                (expand-file-name "blog.js" target)
                t)))

(defun my-static-blog--post-year (post)
  "Return year string for POST."
  (let ((date (or (plist-get post :date) "")))
    (if (string-match "\\`\\([0-9]\\{4\\}\\)" date)
        (match-string 1 date)
      "未归档")))

(defun my-static-blog--post-month-day (post)
  "Return month-day string for POST."
  (let ((date (or (plist-get post :date) "")))
    (if (string-match "\\`[0-9]\\{4\\}-\\([0-9]\\{2\\}-[0-9]\\{2\\}\\)" date)
        (match-string 1 date)
      date)))

(defun my-static-blog--post-list-item (post)
  "Return index list item HTML for POST."
  (let ((title (my-static-blog--html-escape (plist-get post :title)))
        (url (my-static-blog--html-escape (plist-get post :url)))
        (date (my-static-blog--html-escape (my-static-blog--post-month-day post)))
        (description (my-static-blog--html-escape (plist-get post :description))))
    (format "<li><a class=\"post-list-row\" href=\"%s\"><span>%s</span><time>%s</time></a>%s</li>"
            url title date
            (if (string-empty-p description) "" (format "<p>%s</p>" description)))))

(defun my-static-blog--post-year-section (year posts)
  "Return index year section HTML for YEAR and POSTS."
  (format "<section class=\"post-year\"><h2>%s</h2><ol class=\"post-list\">\n%s\n</ol></section>"
          (my-static-blog--html-escape year)
          (mapconcat #'my-static-blog--post-list-item posts "\n")))

(defun my-static-blog--group-posts-by-year (posts)
  "Return POSTS grouped by year, preserving existing post order."
  (let (groups order)
    (dolist (post posts)
      (let ((year (my-static-blog--post-year post)))
        (unless (assoc year groups)
          (push year order)
          (push (list year) groups))
        (setcdr (assoc year groups) (append (cdr (assoc year groups)) (list post)))))
    (mapcar (lambda (year)
              (cons year (cdr (assoc year groups))))
            (nreverse order))))

(defun my-static-blog--render-index (posts)
  "Return the blog index document for POSTS."
  (let ((items (mapconcat (lambda (group)
                            (my-static-blog--post-year-section (car group) (cdr group)))
                          (my-static-blog--group-posts-by-year posts)
                          "\n")))
    (my-static-blog--render-document
     my-static-blog-title
     (concat (my-static-blog--site-header)
             "<main class=\"page\"><h1 class=\"index-title\">"
             (my-static-blog--html-escape my-static-blog-title)
             "</h1><p class=\"index-subtitle\">"
             (my-static-blog--html-escape my-static-blog-subtitle)
             "</p><div class=\"post-archive\">\n"
             items
             "\n</div></main>"
             (my-static-blog--site-footer)))))

(defun my-static-blog--write-index ()
  "Write the blog index page."
  (let ((index-file (expand-file-name "index.html" my-static-blog-public-directory)))
    (make-directory my-static-blog-public-directory t)
    (with-temp-file index-file
      (insert (my-static-blog--render-index (my-static-blog--collect-posts))))))

;;;###autoload
(defun my-static-blog-publish-file (file &optional _force)
  "Publish Org FILE into `my-static-blog-public-directory'."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (unless (and file (file-exists-p file))
    (user-error "Current buffer is not visiting a file"))
  (unless (string-suffix-p ".org" file)
    (user-error "Not an Org file: %s" file))
  (make-directory my-static-blog-public-directory t)
  (let ((output-file (expand-file-name
                      (concat (file-name-base file) ".html")
                      my-static-blog-public-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (my-static-blog--rewrite-id-links-for-export)
      (let* ((org-html-htmlize-output-type (my-static-blog--htmlize-output-type))
             (org-export-with-broken-links t)
             (contents (my-static-blog--remove-cjk-soft-break-spaces
                        (org-export-as 'html nil nil t (my-static-blog--export-options))))
             (post (my-static-blog--read-org-post file)))
        (with-temp-file output-file
          (insert (my-static-blog--render-page contents post))))))
  (my-static-blog--copy-assets)
  (my-static-blog--write-index)
  (when (called-interactively-p 'interactive)
    (message "Published %s to %s" file my-static-blog-public-directory)))

;;;###autoload
(defun my-static-blog-publish (&optional force)
  "Publish the current Org file into the static blog directory.
With FORCE, keep compatibility with older key bindings; the argument is ignored."
  (interactive "P")
  (my-static-blog-publish-file (buffer-file-name) force))

;;;###autoload
(defun my-static-blog-open-public ()
  "Open the generated blog index."
  (interactive)
  (browse-url-of-file (expand-file-name "index.html" my-static-blog-public-directory)))

(provide 'static-blog)
;;; my-static-blog.el ends here
