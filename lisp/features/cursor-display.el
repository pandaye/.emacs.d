;;; my-cursor.el --- Dynamic cursor color management  -*- lexical-binding: t; -*-

;;; Commentary:
;; 终端下根据 Meow 模式状态和 Rime/Rimel 输入法动态改变光标颜色（OSC 12）。
;;
;; 逻辑：
;;   Insert + Rime → 红色（提醒输入法已激活）
;;   Motion        → 橙色
;;   Normal / Insert 无 Rime → 绿色
;;   其他 transient state（keypad/beacon/...）→ 保持上次颜色，
;;     避免短暂态把 insert+rime 红色覆盖成绿色。
;;
;; 关键防御：
;; - meow-switch-state-hook 可能在后台 buffer（如 *diff-hl-diff*）触发，
;;   回调内必须确认 current-buffer == selected-window 的可见 buffer，
;;   否则其他 buffer 的 state 切换会污染当前光标颜色。
;; - window-* hook 回调时 current-buffer 不可控，必须用
;;   `(window-buffer (selected-window))` + `with-current-buffer` 显式定位。
;;
;; 输入法状态检测：
;;   Rimel 直接注册为原生 input method，不提供 `rime-mode' 这类 minor mode。
;;   因此统一以 `current-input-method' 为准，兼容后续输入法实现切换。

;;; Code:

;; ── 颜色配置 ─────────────────────────────────────────────

(defvar my/cursor-color-rime   "#FF6B6B" "Insert + Rime 激活时的光标颜色。")
(defvar my/cursor-color-motion "#FFA500" "Meow motion 模式的光标颜色。")
(defvar my/cursor-color-normal "#00FF00" "Meow normal / insert 无 Rime 的光标颜色。")

;; ── Rime 状态检测 ─────────────────────────────────────────

(defun my/rime-active-p ()
  "Return non-nil if a Rime-family input method is active in the current buffer."
  (member current-input-method '("rime" "rimel")))

;; ── 核心函数 ─────────────────────────────────────────────

(defun my/wezterm-set-cursor-color (color)
  "通过 OSC 12 设置终端光标颜色。COLOR 为十六进制字符串如 \"#FF0000\"。"
  (unless (display-graphic-p)
    (send-string-to-terminal (format "\e]12;%s\a" color))))

(defvar my/cursor--last-color nil
  "上次设置的光标颜色，避免重复发送 OSC 序列。")

(defun my/cursor--set-color (color)
  "仅在颜色变化时发送 OSC 12。COLOR 为 nil 时保持当前颜色不变。"
  (when (and color (not (equal color my/cursor--last-color)))
    (setq my/cursor--last-color color)
    (my/wezterm-set-cursor-color color)))

(defun my/cursor--compute-color (state)
  "根据 STATE 和 Rime 状态计算颜色。
未知 state（keypad/beacon/...）返回 nil，表示保持上次颜色不变，
避免短暂态污染 insert+rime 红色。"
  (cond
   ((and (eq state 'insert) (my/rime-active-p))
    my/cursor-color-rime)
   ((eq state 'insert)               ; insert 无 rime
    my/cursor-color-normal)
   ((eq state 'motion)
    my/cursor-color-motion)
   ((eq state 'normal)
    my/cursor-color-normal)
   (t nil)))                          ; keypad/beacon/未知 → 保持

(defun my/update-cursor-color-for-state (state)
  "meow-switch-state-hook 回调。STATE 为新 state。
仅当 current-buffer 为 selected-window 的可见 buffer 时才处理，
避免后台 buffer（如 *diff-hl-diff*）的 state 切换污染光标颜色。"
  (when (eq (current-buffer) (window-buffer (selected-window)))
    (my/cursor--set-color (my/cursor--compute-color state))))

(defun my/update-cursor-color-for-buffer (&optional _frame)
  "窗口 / buffer / 输入法切换时，根据 selected-window 的 buffer 重新判定。"
  (with-current-buffer (window-buffer (selected-window))
    (when (bound-and-true-p meow-mode)
      (my/cursor--set-color (my/cursor--compute-color (meow--current-state))))))

(defun my/refresh-cursor-color-after-input-method (&rest _args)
  "Refresh cursor color after input-method related commands."
  (my/update-cursor-color-for-buffer))

(provide 'cursor-display)
;;; my-cursor.el ends here
