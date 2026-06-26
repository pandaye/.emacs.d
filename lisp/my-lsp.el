;; -*- lexical-binding: t; -*-
;;; my-lsp.el --- LSP Bridge 配置（延迟加载）

;;; Commentary:
;; LSP 配置延迟加载，仅在进入编程模式时激活。
;; 通过 pandaye-init.el 中的 hook 触发加载。
;;
;; lsp-bridge 不支持标准 package-vc（无 autoload cookies），
;; 使用 package-vc-install 拉取源码 + 手动 load-path 方式管理。

;;; Code:

;; 确保 lsp-bridge 仓库已克隆到 elpa 目录
(let ((lsp-bridge-dir (expand-file-name "elpa/lsp-bridge" user-emacs-directory)))
  (unless (file-exists-p (expand-file-name "lsp-bridge.el" lsp-bridge-dir))
    (package-vc-install '(lsp-bridge :url "https://github.com/manateelazycat/lsp-bridge")))
  (add-to-list 'load-path lsp-bridge-dir))

(defun my-lsp-bridge-not-common-lisp-buffer ()
  "Return nil in Common Lisp buffers handled by Corfu."
  (not (memq major-mode '(lisp-mode slime-repl-mode))))

(defvar-local my-acm-candidate-selected nil)

(defun my-acm-mark-candidate-selected (&rest _)
  "Remember that the user explicitly selected an ACM candidate."
  (setq-local my-acm-candidate-selected t))

(defun my-acm-reset-candidate-selection (&rest _)
  "Reset explicit ACM selection state."
  (setq-local my-acm-candidate-selected nil))

(defun my-acm-unselect-candidate-after-update (&rest _)
  "Keep ACM prompt unselected until the user explicitly selects a candidate."
  (when (and (not my-acm-candidate-selected)
             (boundp 'acm-menu-index)
             (>= acm-menu-index 0))
    (setq-local acm-menu-index -1)
    (when (overlayp acm-preview-overlay)
      (delete-overlay acm-preview-overlay)
      (setq acm-preview-overlay nil))
    (when (and (boundp 'acm-menu-candidates)
               acm-menu-candidates
               (fboundp 'acm-menu-render))
      (acm-menu-render (cons acm-menu-max-length-cache acm-menu-number-cache)))))

(defun my-acm-return-or-newline ()
  "Complete selected ACM candidate, or insert newline if none is selected."
  (interactive)
  (if (and (boundp 'acm-menu-index)
           (>= acm-menu-index 0))
      (acm-complete)
    (acm-hide)
    (if (minibufferp)
        (exit-minibuffer)
      (newline))))

(defun my-acm-tab-complete-first ()
  "Complete selected ACM candidate, selecting the first candidate if needed."
  (interactive)
  (my-acm-mark-candidate-selected)
  (when (and (boundp 'acm-menu-index)
             (< acm-menu-index 0))
    (setq-local acm-menu-index 0))
  (acm-complete))

(use-package lsp-bridge
  :ensure nil
  :demand t
  :custom
  (acm-enable-copilot t)
  (tty-child-frames t)
  (acm-enable-icon nil)
  (c-basic-offset 4)
  (lsp-bridge-user-langserver-dir (expand-file-name "lsp-bridge-langserver" user-emacs-directory))
  :config
  (setq lsp-bridge-get-project-path-by-filepath
        (lambda (filename)
          "对 Go 文件优先查找 go.mod，其他文件使用默认行为"
          (if (string-match-p "\\.go\\'" filename)
              (or (when-let* ((go-mod-dir (locate-dominating-file filename "go.mod")))
                    (expand-file-name go-mod-dir))
                (when-let* ((git-dir (locate-dominating-file filename ".git")))
                  (expand-file-name git-dir)))
            (let* ((result (dir-locals-find-file filename))
                   (dir (if (consp result) (car result) result)))
              (when dir (directory-file-name dir))))))

  (setq lsp-bridge-get-single-lang-server-by-project
        (lambda (project-path filepath)
          ;; 存在项目根目录下的 .lsp-bridge.json 则使用该配置文件
          (let ((custom-config (expand-file-name ".lsp-bridge.json" project-path)))
            (when (file-exists-p custom-config)
              custom-config))))

  (add-to-list 'lsp-bridge-enable-predicates
               #'my-lsp-bridge-not-common-lisp-buffer)

  (with-eval-after-load 'acm
    (add-to-list 'acm-continue-commands #'my-acm-return-or-newline)
    (add-to-list 'acm-continue-commands #'my-acm-tab-complete-first)
    (define-key acm-mode-map (kbd "RET") #'my-acm-return-or-newline)
    (define-key acm-mode-map (kbd "<return>") #'my-acm-return-or-newline)
    (define-key acm-mode-map "\C-m" #'my-acm-return-or-newline)
    (define-key acm-mode-map "\n" #'my-acm-return-or-newline)
    (define-key acm-mode-map (kbd "TAB") #'my-acm-tab-complete-first)
    (define-key acm-mode-map "\t" #'my-acm-tab-complete-first)
    (advice-add 'acm-update :after #'my-acm-unselect-candidate-after-update)
    (advice-add 'acm-hide :after #'my-acm-reset-candidate-selection)
    (dolist (command '(acm-select-first acm-select-last acm-select-next acm-select-prev
                       acm-select-next-page acm-select-prev-page))
      (advice-add command :before #'my-acm-mark-candidate-selected)))

  ;; 修复补全弹窗错位：上游 acm-frame-get-popup-position 混用
  ;; window-pixel-edges（含行号列）与 posn-at-point（文本区域相对），
  ;; 导致开启 display-line-numbers-mode 时弹窗向右偏移行号列宽度。
  ;; 改用 window-body-pixel-edges 统一坐标系。
  (define-advice acm-frame-get-popup-position
      (:override (frame-popup-point &optional line-bias)
                 fix-line-number-offset)
    "Use body-pixel-edges to align popup with text area coordinates."
    (let* ((edges (window-body-pixel-edges))
           (window-left
            (+ (nth 0 edges)
               ;; Icon fine-tuning: shift left when icons disabled.
               (if (bound-and-true-p acm-enable-icon)
                   0
                 (* (frame-char-width)
                    (1- (or (bound-and-true-p acm-icon-width) 0))))
               ;; Quick-access index fine-tuning.
               (if (bound-and-true-p acm-enable-quick-access)
                   (- (* (frame-char-width) 3))
                 0)))
           (window-top (nth 1 edges))
           (pos (posn-x-y (posn-at-point frame-popup-point)))
           (x (car pos))
           (y (+ (cdr pos) (* (or line-bias 0) (line-pixel-height))))
           (offset-y
            (+ (window-tab-line-height)
               (window-header-line-height))))
      (cons (+ x window-left)
            (+ y window-top offset-y))))

  (global-lsp-bridge-mode)

  :bind
  (("C-c r d" . lsp-bridge-find-def)
   ("C-c r t" . lsp-bridge-find-type-def)
   ("C-c r r" . lsp-bridge-find-def-return)
   ("C-c r i" . lsp-bridge-find-impl-other-window)
   ("C-c r R" . lsp-bridge-find-references)
   ("C-c r s" . lsp-bridge-show-documentation)
   ("C-c r f" . lsp-bridge-code-format)
   ("C-c r h" . lsp-bridge-diagnostic-list)))

(provide 'my-lsp)
;;; my-lsp.el ends here
