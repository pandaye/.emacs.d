;; -*- lexical-binding: t; -*-
;;; my-rime.el --- Rime 中文输入法配置

;;; Commentary:
;; Rimel 输入法配置，包括输入方案切换。
;; 光标颜色联动由 my-cursor.el 统一管理。

;;; Code:

(use-package rimel
  :defer t)

(use-package posframe
  :defer rimel)

(setq liberime-user-data-dir (locate-user-emacs-file "rime/")
      liberime-auto-build t
      default-input-method "rimel"
      rimel-schema "tigress"
      rimel-posframe-style 'horizontal
      rimel-disable-predicates '(meow-not-insert-p
                                 my/rimel-predicate-after-ascii-nonspace-p
                                 my/rimel-predicate-space-after-cjk-p
                                 rimel-predicate-after-alphabet-char-p
                                 rimel-predicate-prog-in-code-p))

(defun my/ensure-rimel-loaded ()
  "Load Rimel and Liberime on demand."
  (unless (featurep 'liberime)
    (require 'liberime))
  (unless (featurep 'rimel)
    (require 'rimel)))

(defun my/set-rimel-schema (schema)
  "Switch Rimel schema to SCHEMA and persist it for later activations."
  (my/ensure-rimel-loaded)
  (setq rimel-schema schema)
  (when (and (equal current-input-method "rimel")
             (fboundp 'liberime-try-select-schema))
    (liberime-try-select-schema schema)))

(defun my/rimel-toggle-ascii-punct ()
  "Toggle Rime ascii punctuation for the active Rimel session."
  (interactive)
  (my/ensure-rimel-loaded)
  (unless (equal current-input-method "rimel")
    (user-error "Current input method is not rimel"))
  (unless (fboundp 'liberime-process-keys)
    (user-error "liberime is not available"))
  (liberime-process-keys (kbd "C-."))
  (message "Sent C-. to Rimel"))

(defun my/rimel-predicate-after-ascii-nonspace-p ()
  "光标前是可打印 ASCII 非空字符时，不触发候选。
遇到空格、换行、回车、制表等空白字符后恢复 Rime。"
  (let ((ch (char-before)))
    (and ch (>= ch 33) (<= ch 126))))

(defun my/rimel-predicate-space-after-cjk-p ()
  "光标前是「CJK + 空格」时不触发候选。
中文句子里出现空格通常意味着接下来要敲英文，避免后续字母被当作拼音。"
  (let ((p (point)))
    (and (> p (+ (point-min) 1))
         (eq (char-before p) ?\s)
         (let ((ch (char-before (1- p))))
           (and ch
                (or (and (>= ch #x3400) (<= ch #x9FFF))    ; CJK 基本 + 扩展 A
                    (and (>= ch #x20000) (<= ch #x2FFFF))  ; 扩展 B/C/D/E/F
                    (and (>= ch #x3000) (<= ch #x303F))    ; CJK 符号与标点
                    (and (>= ch #xFF00) (<= ch #xFFEF)))))))) ; 全角 ASCII / 标点

(defun my/set-rime-jp ()
  "切换到日语输入方案。"
  (interactive)
  (my/set-rimel-schema "jaroomaji"))

(defun my/set-rime-zh ()
  "切换到中文输入方案。"
  (interactive)
  (my/set-rimel-schema "tigress"))

(with-eval-after-load 'liberime
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (fboundp 'liberime-finalize)
                (liberime-finalize)))))

;; 输入法切换快捷键
(global-set-key (kbd "C-c i i") 'toggle-input-method)
(global-set-key (kbd "C-c i j") #'my/set-rime-jp)
(global-set-key (kbd "C-c i f") #'my/set-rime-zh)
(global-set-key (kbd "C-c .") #'my/rimel-toggle-ascii-punct)
(global-set-key (kbd "C-.") #'my/rimel-toggle-ascii-punct)

(provide 'my-rime)
;;; my-rime.el ends here
