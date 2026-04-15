;;; my-cursor.el --- Dynamic cursor color management  -*- lexical-binding: t; -*-

;;; Commentary:
;; 终端下根据 Meow 模式状态和 Rime 输入法动态改变光标颜色（OSC 12）。
;;
;; 优先级：Rime 激活 > Meow 状态（motion / normal / insert）
;; 颜色方案：
;;   Rime   → 红色   #FF6B6B
;;   Motion → 橙色   #FFA500
;;   Normal → 绿色   #00FF00
;;   Insert → 白色   #FFFFFF
;;
;; 本模块是光标颜色的唯一管理者，统一注册所有触发 hook。

;;; Code:

;; ── 颜色配置 ─────────────────────────────────────────────

(defvar my/cursor-color-rime   "#FF6B6B" "Rime 输入法激活时的光标颜色。")
(defvar my/cursor-color-motion "#FFA500" "Meow motion 模式的光标颜色。")
(defvar my/cursor-color-normal "#00FF00" "Meow normal 模式的光标颜色。")
(defvar my/cursor-color-insert "#FFFFFF" "Meow insert 模式的光标颜色。")

;; ── 核心函数 ─────────────────────────────────────────────

(defun my/wezterm-set-cursor-color (color)
  "通过 OSC 12 设置终端光标颜色。COLOR 为十六进制字符串如 \"#FF0000\"。"
  (unless (display-graphic-p)
    (send-string-to-terminal (format "\e]12;%s\a" color))))

(defun my/update-cursor-color ()
  "根据 Rime 和 Meow 当前状态更新光标颜色。
优先级：Rime > Meow motion > Meow normal > Meow insert > 默认(normal)。"
  (interactive)
  (my/wezterm-set-cursor-color
   (cond
    ;; Rime 激活
    ((and (boundp 'rime-mode) rime-mode)
     my/cursor-color-rime)
    ;; Meow 状态
    ((and (fboundp 'meow--current-state) (bound-and-true-p meow-mode))
     (pcase (meow--current-state)
       ('motion my/cursor-color-motion)
       ('insert my/cursor-color-insert)
       (_       my/cursor-color-normal)))
    ;; 无 meow / 非 meow buffer
    (t my/cursor-color-normal))))

(defun my/update-cursor-color-on-frame-change (_frame)
  "窗口 / buffer 变化时更新光标颜色（hook 回调，忽略 FRAME 参数）。"
  (my/update-cursor-color))

;; ── Hook 注册 ────────────────────────────────────────────

;; Meow 状态切换（normal ↔ insert ↔ motion）
(with-eval-after-load 'meow
  (add-hook 'meow-state-change-hook #'my/update-cursor-color))

;; Rime 开关
(with-eval-after-load 'rime
  (add-hook 'rime-mode-hook #'my/update-cursor-color))

;; 窗口 / buffer 切换（不同 buffer 可能处于不同 meow 状态）
(add-hook 'window-buffer-change-functions    #'my/update-cursor-color-on-frame-change)
(add-hook 'window-selection-change-functions #'my/update-cursor-color-on-frame-change)

(provide 'my-cursor)
;;; my-cursor.el ends here
