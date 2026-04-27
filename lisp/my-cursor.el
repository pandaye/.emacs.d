;;; my-cursor.el --- Dynamic cursor color management  -*- lexical-binding: t; -*-

;;; Commentary:
;; 终端下根据 Meow 模式状态和 Rime 输入法动态改变光标颜色（OSC 12）。
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
;; Rime 状态检测：
;;   不能仅依赖 `rime-mode'，因为它是 buffer-local minor mode，
;;   在新 buffer 中默认为 nil（:init-value nil），只有 rime-activate
;;   才会将其设为 t。切 buffer 后 rime-mode 可能为 nil 即使
;;   current-input-method 仍然是 "rime"，导致光标颜色不正确。
;;   因此同时检查 `current-input-method'，与 rime 自身的
;;   rime-lighter 保持一致的判断逻辑。

;;; Code:

;; ── 颜色配置 ─────────────────────────────────────────────

(defvar my/cursor-color-rime   "#FF6B6B" "Insert + Rime 激活时的光标颜色。")
(defvar my/cursor-color-motion "#FFA500" "Meow motion 模式的光标颜色。")
(defvar my/cursor-color-normal "#00FF00" "Meow normal / insert 无 Rime 的光标颜色。")

;; ── Rime 状态检测 ─────────────────────────────────────────

(defun my/rime-active-p ()
  "Return non-nil if Rime is the active input method in the current buffer.
同时检查 `rime-mode' 和 `current-input-method'：
- `rime-mode' 在首次 toggle input method 后才为 t（buffer-local）
- `current-input-method' 反映 Emacs 的 input method 激活状态
二者任一为 rime 即视为激活，与 rime-lighter 判断逻辑一致。"
  (or (bound-and-true-p rime-mode)
      (equal current-input-method "rime")))

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