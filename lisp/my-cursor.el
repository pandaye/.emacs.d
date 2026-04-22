;;; my-cursor.el --- Dynamic cursor color management  -*- lexical-binding: t; -*-

;;; Commentary:
;; 终端下根据 Meow 模式状态和 Rime 输入法动态改变光标颜色（OSC 12）。
;;
;; 逻辑：
;;   Insert + Rime → 红色（提醒输入法已激活）
;;   Motion        → 橙色
;;   其他          → 绿色
;;
;; meow-switch-state-hook 传入新 state 作为参数，hook 在状态切换完成后触发。

;;; Code:

;; ── 颜色配置 ─────────────────────────────────────────────

(defvar my/cursor-color-rime   "#FF6B6B" "Insert + Rime 激活时的光标颜色。")
(defvar my/cursor-color-motion "#FFA500" "Meow motion 模式的光标颜色。")
(defvar my/cursor-color-normal "#00FF00" "Meow normal / insert 无 Rime 的光标颜色。")

;; ── 核心函数 ─────────────────────────────────────────────

(defun my/wezterm-set-cursor-color (color)
  "通过 OSC 12 设置终端光标颜色。COLOR 为十六进制字符串如 \"#FF0000\"。"
  (unless (display-graphic-p)
    (send-string-to-terminal (format "\e]12;%s\a" color))))

(defvar my/cursor--last-color nil
  "上次设置的光标颜色，避免重复发送 OSC 序列。")

(defun my/cursor--set-color (color)
  "仅在颜色变化时发送 OSC 12。"
  (unless (equal color my/cursor--last-color)
    (setq my/cursor--last-color color)
    (my/wezterm-set-cursor-color color)))

(defun my/update-cursor-color-for-state (state)
  "根据 STATE 和 Rime 状态更新光标颜色。
STATE 由 meow-switch-state-hook 传入。"
  (my/cursor--set-color
   (cond
    ((and (eq state 'insert) (bound-and-true-p rime-mode))
     my/cursor-color-rime)
    ((eq state 'motion)
     my/cursor-color-motion)
    (t my/cursor-color-normal))))

(defun my/update-cursor-color-for-buffer (&optional _frame)
  "窗口 / buffer 切换时，根据当前 buffer 的 meow state 更新光标颜色。"
  (when (bound-and-true-p meow-mode)
    (my/update-cursor-color-for-state (meow--current-state))))

;; ── Hook 注册 ────────────────────────────────────────────

;; meow-switch-state-hook: run-hook-with-args 传入新 state
(with-eval-after-load 'meow
  (add-hook 'meow-switch-state-hook #'my/update-cursor-color-for-state))

;; Insert 下切换 Rime 时重新判断颜色
(with-eval-after-load 'rime
  (add-hook 'rime-mode-hook #'my/update-cursor-color-for-buffer))

;; 窗口 / buffer 切换
(add-hook 'window-buffer-change-functions    #'my/update-cursor-color-for-buffer)
(add-hook 'window-selection-change-functions #'my/update-cursor-color-for-buffer)

(provide 'my-cursor)
;;; my-cursor.el ends here
