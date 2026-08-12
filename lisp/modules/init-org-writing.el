;; -*- lexical-binding: t; -*-
;;; init-org-writing.el --- Org 外观美化配置

;;; Commentary:
;; Org 模式的外观设置，包括引用块美化、代码块标题低可视度等。
;; 其他 Org 功能已拆分到独立模块：
;; - init-input-method.el: 中文输入法配置
;; - init-org-gtd.el: GTD 任务管理
;; - org-diary.el: 日记系统
;; - init-org-roam.el: 双向链接笔记

;;; Code:

;; ============================================================
;; Org 外观美化
;; ============================================================

;; 美化引用和诗词块
(setq org-fontify-quote-and-verse-blocks t)

;; 低可视度 block 标题（Org 加载后设置，避免面部未定义错误）
(with-eval-after-load 'org
  (set-face-attribute 'org-quote nil :foreground "gray70" :extend t)
  (set-face-attribute 'org-block-begin-line nil :foreground "gray35")
  (set-face-attribute 'org-block-end-line nil :foreground "gray35"))

;; 其他未归档的配置
(font-lock-add-keywords
 'org-mode
 '(("^ *- State \\(\"[A-Z]+\"\\).*\\[.*\\]"
    0 'font-lock-comment-face t)))

(provide 'init-org-writing)
;;; init-org-writing.el ends here
