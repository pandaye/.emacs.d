;;; init-dsh.el --- DeepSeek Harness (dsh) client -*- lexical-binding: t; -*-

;;; Commentary:
;; dsh-emacs 是 dsh（DeepSeek Harness）的 Emacs 客户端。
;; 用 `:vc' 从 GitHub 安装（package-vc），按需加载：启动阶段只注册 autoload，
;; 不 require 整个客户端。更新用 `M-x package-vc-upgrade-all'。

;;; Code:

(use-package dsh-emacs
  ;; 装自己的 fork（pandaye/dsh-emacs），不再直接跟上游 vritser/dsh-emacs：
  ;; 这样给客户端打补丁时能直接提交、并让 package-vc 从 fork 拉取。
  ;; fork 与上游同步：`gh repo sync pandaye/dsh-emacs --source vritser/dsh-emacs --branch main'
  ;;
  ;; `:rev "main"' 让它跟 main 的最新提交走；只写 `:branch' 时
  ;; use-package 默认装“最后一个 release”（会停在 v0.7.0 tag 上）。
  :vc (:url "https://github.com/pandaye/dsh-emacs.git" :branch "main" :rev "main")
  :ensure nil
  :commands (dsh-emacs dsh-emacs-new-session))

;; 历史用户消息：整行底色，和助手输出在视觉上分开（文字颜色保持默认）。
;; 底色故意不用代码块的 #32302f，改成带蓝调的 #263238（浅色 #e6f4f7），
;; 这样用户消息是冷色调、代码块是暖灰，两种面板不会混。
(with-eval-after-load 'dsh-emacs
  (custom-set-faces
   '(dsh-emacs-user-block-face
     ((((background light)) :background "#e6f4f7" :extend t)
      (((background dark))  :background "#263238" :extend t))))

  ;; `:extend t' 只在“行尾换行符本身也带这个 face”时才会把背景铺到窗口右缘；
  ;; dsh-emacs 只把 face 刷到正文最后一个字符，所以默认底色贴着文字就断了。
  ;; 这里补一条 advice：把 face 延伸到紧跟正文的那个换行符（分隔用的空行仍不染色），
  ;; 用户消息就变成和代码块一样的整行长条。
  ;; 上游更干净的做法是在 `dsh-emacs-render--insert-chat-message' 里直接带这 1 个字符。
  (defun pandaye/dsh--user-rail-tint (start end)
    "把终端下用户消息左侧的 `│' 换成消息底色，视觉上并入同一块面板。
`dsh-emacs-render--insert-chat-message' 用 `dsh-emacs-reply-meta-face' 画那条
竖条，而这个 face 还管补全摘要和 reply footer，所以这里只重写本行的
`line-prefix'，不动 face 本身。"
    (let ((prefix (get-text-property start 'line-prefix))
          (band (face-attribute 'dsh-emacs-user-block-face :background nil t)))
      (when (and (stringp prefix) (stringp band))
        (let ((tinted (propertize (substring-no-properties prefix)
                                  'face (list :foreground band :background band)))
              (inhibit-read-only t))
          (put-text-property start end 'line-prefix tinted)
          (put-text-property start end 'wrap-prefix tinted)))))

  (defun pandaye/dsh--user-band-extend (fn text face insert-point event-id
                                           &optional role)
    (let ((ret (funcall fn text face insert-point event-id role)))
      (when (and (eq role 'user) (consp ret))
        (when (and (not (eq (char-before (cdr ret)) ?\n))
                   (eq (char-after (cdr ret)) ?\n))
          (let ((inhibit-read-only t))
            (add-face-text-property (cdr ret) (1+ (cdr ret)) face)))
        (unless (display-graphic-p)
          (pandaye/dsh--user-rail-tint (car ret) (cdr ret))))
      ret))

  (advice-add 'dsh-emacs-render--insert-chat-message
              :around #'pandaye/dsh--user-band-extend))

(provide 'init-dsh)
;;; init-dsh.el ends here
