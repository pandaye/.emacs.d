;;; my-cursor.el --- Change cursor color when emacs-rime is active

;; 定义光标颜色
(defvar my/rime-cursor-color "#FF6B6B"
  "Cursor color when rime input method is active.")

(defvar my/default-cursor-color "#00FF00"
  "Default cursor color when rime is inactive.")

(defun my/wezterm-set-cursor-color (color)
  "Set cursor color in WezTerm using OSC escape sequences."
  (unless (display-graphic-p)
    (send-string-to-terminal (format "\e]12;%s\a" color))))

(defun my/rime-wezterm-cursor-on ()
  "Change cursor color for WezTerm when rime is active."
  (my/wezterm-set-cursor-color my/rime-cursor-color))

(defun my/rime-wezterm-cursor-off ()
  "Restore cursor color for WezTerm when rime is inactive."
  (my/wezterm-set-cursor-color my/default-cursor-color))

;; 检查当前 buffer 的 rime 状态并更新光标颜色
(defun my/update-cursor-by-rime-state ()
  "Update cursor color based on current rime state."
  (interactive)
  (if rime-mode
      (my/rime-wezterm-cursor-on)
    (my/rime-wezterm-cursor-off)))

(provide 'my-cursor)
;;; emacs-rime-cursor.el ends here
