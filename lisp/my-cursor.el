;;; my-cursor.el --- Dynamic cursor color management for Rime and Meow  -*- lexical-binding: t; -*-

;;; Commentary:
;; 根据 Rime 输入法和 Meow 模式状态动态改变 WezTerm 光标颜色
;; - Rime 激活时：红色
;; - Meow Motion 模式：橙色（只读/浏览模式）
;; - 默认状态：绿色

;;; Code:

;; 光标颜色配置
(defvar my/rime-cursor-color "#FF6B6B"
  "Cursor color when Rime input method is active (Chinese input).")

(defvar my/default-cursor-color "#00FF00"
  "Default cursor color for normal editing mode.")

(defvar my/motion-cursor-color "#FFA500"
  "Cursor color for Meow motion mode (read-only/browse mode).")

;; 核心函数：设置 WezTerm 光标颜色
(defun my/wezterm-set-cursor-color (color)
  "Set cursor color in WezTerm using OSC escape sequences.
COLOR should be a hex color string like \"#FF0000\"."
  (unless (display-graphic-p)
    (send-string-to-terminal (format "\e]12;%s\a" color))))

;; 光标状态切换函数
(defun my/set-rime-cursor ()
  "Set cursor color for Rime input mode."
  (my/wezterm-set-cursor-color my/rime-cursor-color))

(defun my/set-motion-cursor ()
  "Set cursor color for Meow motion mode."
  (my/wezterm-set-cursor-color my/motion-cursor-color))

(defun my/set-default-cursor ()
  "Set cursor color to default state."
  (my/wezterm-set-cursor-color my/default-cursor-color))

;; 主更新函数：根据当前状态更新光标颜色
(defun my/update-cursor-by-rime-state ()
  "Update cursor color based on current Rime and Meow state.
Priority: Rime mode > Meow motion mode > Default."
  (interactive)
  (cond
   ;; 优先级 1: Rime 输入法激活
   ((and (boundp 'rime-mode) rime-mode)
    (my/set-rime-cursor))
   ;; 优先级 2: Meow motion 模式
   ((and (featurep 'meow) (meow-motion-mode-p))
    (my/set-motion-cursor))
   ;; 优先级 3: 默认状态
   (t
    (my/set-default-cursor))))

;; 光标颜色跟随状态自动更新
;; 统一处理 buffer 和窗口变化时的光标更新
(defun my/update-cursor-on-frame-change (frame)
  "Update cursor color when FRAME is the selected frame."
  (when (eq frame (selected-frame))
    (my/update-cursor-by-rime-state)))

(provide 'my-cursor)
;;; my-cursor.el ends here
