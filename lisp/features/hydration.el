;; -*- lexical-binding: t; -*-
;;; hydration.el --- Periodic water drinking reminder via tmux window name

;; Author: Pandaye
;; Keywords: tmux, convenience, health

;;; Commentary:
;; Reminds the user to drink water on a fixed interval: renames tmux window 0
;; to "要喝水", shows an echo-area message and a mode-line droplet while a
;; reminder is pending.  `hydration-confirm' restores the original window name
;; and re-arms the next cycle.  No history is recorded.

;;; Code:

(require 'tmux-manager)

(defgroup hydration nil
  "Periodic water drinking reminder."
  :group 'tools
  :prefix "hydration-")

(defcustom hydration-interval 45
  "Minutes between hydration reminders."
  :type 'integer
  :group 'hydration)

(defcustom hydration-reminder-name "💧 要喝水"
  "Tmux window 0 name used while a reminder is pending."
  :type 'string
  :group 'hydration)

(defvar hydration--timer nil
  "Active `run-at-time' timer object, or nil when not running.")

(defvar hydration--original-name nil
  "Original tmux window 0 name saved at `hydration-start' time.")

(defvar hydration--active nil
  "Non-nil while a water reminder is pending.")

(defun hydration--session ()
  "Return current tmux session name, or nil when tmux is unavailable."
  (when (tmux-manager--tmux-available-p)
    (tmux-manager--get-current-session)))

(defun hydration--window-0-name (session)
  "Return the name of tmux window 0 in SESSION, or nil."
  (tmux-manager--run-tmux-command
   "display-message" "-p" "-t" (format "%s:0" session) "#{window_name}"))

(defun hydration--rename-window-0 (session name)
  "Rename tmux window 0 in SESSION to NAME."
  (tmux-manager--run-tmux-command
   "rename-window" "-t" (format "%s:0" session) name))

(defun hydration--arm-timer ()
  "Schedule the next one-shot reminder timer after `hydration-interval'."
  (setq hydration--timer
        (run-at-time (* hydration-interval 60) nil #'hydration--fire)))

(defun hydration--mode-line-indicator ()
  "Return mode-line text reflecting the current hydration state."
  (if hydration--active " 💧 " ""))

(defun hydration-start ()
  "Start the periodic water drinking reminder cycle."
  (interactive)
  (when (timerp hydration--timer)
    (cancel-timer hydration--timer))
  (let ((session (hydration--session)))
    (setq hydration--original-name
          (and session (hydration--window-0-name session))))
  (setq hydration--active nil)
  (hydration--arm-timer)
  (force-mode-line-update)
  (message "💧 喝水提醒已启动，每 %d 分钟提醒一次" hydration-interval))

(defun hydration--fire ()
  "Fire the hydration reminder: rename window, message, and mark active."
  (setq hydration--timer nil)
  (setq hydration--active t)
  (let ((session (hydration--session)))
    (when (and session hydration--original-name)
      (hydration--rename-window-0 session hydration-reminder-name)))
  (force-mode-line-update)
  (message "💧 该喝水啦！喝完后执行 M-x hydration-confirm"))

(defun hydration-confirm ()
  "Confirm the water was drunk, restore window name and re-arm the cycle."
  (interactive)
  (if hydration--active
      (progn
        (let ((session (hydration--session)))
          (when (and session hydration--original-name)
            (hydration--rename-window-0 session hydration--original-name)))
        (setq hydration--active nil)
        (when (timerp hydration--timer)
          (cancel-timer hydration--timer))
        (hydration--arm-timer)
        (force-mode-line-update)
        (message "已确认喝水，下一轮提醒将在 %d 分钟后" hydration-interval))
    (message "当前没有待确认的喝水提醒")))

(defun hydration-cancel ()
  "Cancel the reminder cycle and restore the tmux window name."
  (interactive)
  (when (timerp hydration--timer)
    (cancel-timer hydration--timer)
    (setq hydration--timer nil))
  (let ((session (hydration--session)))
    (when (and session hydration--original-name)
      (hydration--rename-window-0 session hydration--original-name)))
  (setq hydration--active nil)
  (force-mode-line-update)
  (message "喝水提醒已取消"))

(setq-default mode-line-format
              (append mode-line-format
                      (list '(:eval (hydration--mode-line-indicator)))))

(provide 'hydration)
;;; hydration.el ends here
