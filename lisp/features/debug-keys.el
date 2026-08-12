;;; debug-keys.el --- Debug keybinding issues -*- lexical-binding: t; -*-

;;; Commentary:
;; 用于诊断按键绑定问题的工具

;;; Code:

(defvar debug-keys--last-command nil
  "记录上一次执行的命令。")

(defvar debug-keys--enabled nil
  "是否启用按键调试。")

(defun debug-keys--post-command ()
  "在命令执行后记录信息。"
  (when debug-keys--enabled
    (setq debug-keys--last-command this-command)
    (message "[DEBUG-KEYS] Command: %s | Keys: %s | Real-this-command: %s" 
             this-command
             (key-description (this-command-keys-vector))
             real-this-command)))

(defun debug-keys-enable ()
  "启用按键调试模式。"
  (interactive)
  (setq debug-keys--enabled t)
  (add-hook 'post-command-hook #'debug-keys--post-command)
  (message "按键调试已启用。每次按键后会在 minibuffer 显示执行的命令。"))

(defun debug-keys-disable ()
  "禁用按键调试模式。"
  (interactive)
  (setq debug-keys--enabled nil)
  (remove-hook 'post-command-hook #'debug-keys--post-command)
  (message "按键调试已禁用。"))

(defun debug-keys-show-binding (key-sequence)
  "显示 KEY-SEQUENCE 在当前 buffer 中的详细绑定信息。"
  (interactive "kPress key sequence: ")
  (let* ((key-desc (key-description key-sequence))
         (local-map (current-local-map))
         (global-binding (global-key-binding key-sequence))
         (local-binding (when local-map (lookup-key local-map key-sequence)))
         (minor-mode-bindings '()))
    
    ;; 检查所有活动的 minor mode
    (dolist (mode minor-mode-map-alist)
      (when (and (boundp (car mode)) (symbol-value (car mode)))
        (let ((binding (lookup-key (cdr mode) key-sequence)))
          (when (and binding (not (numberp binding)))
            (push (cons (car mode) binding) minor-mode-bindings)))))
    
    ;; 显示结果
    (with-output-to-temp-buffer "*Key Binding Debug*"
      (princ (format "Key Sequence: %s\n\n" key-desc))
      (princ "=== Global Binding ===\n")
      (princ (format "%s\n\n" (or global-binding "none")))
      (princ "=== Local Map Binding (current major mode) ===\n")
      (princ (format "%s\n\n" (or local-binding "none")))
      (princ "=== Active Minor Mode Bindings ===\n")
      (if minor-mode-bindings
          (dolist (binding minor-mode-bindings)
            (princ (format "%s: %s\n" (car binding) (cdr binding))))
        (princ "none\n"))
      (princ "\n=== Effective Binding (what will actually execute) ===\n")
      (princ (format "%s\n" (key-binding key-sequence))))))

(defun debug-keys-trace-org-archive ()
  "专门用于诊断 org-archive 的问题。"
  (interactive)
  (debug-keys-show-binding (kbd "C-c C-x C-a"))
  (with-current-buffer "*Key Binding Debug*"
    (goto-char (point-max))
    (insert "\n=== Org Archive Functions ===\n")
    (insert (format "org-archive-subtree-default: %s\n" 
                    (if (fboundp 'org-archive-subtree-default) "defined" "NOT DEFINED")))
    (insert (format "append-to-file: %s\n" 
                    (if (fboundp 'append-to-file) "defined" "NOT DEFINED")))))

(provide 'debug-keys)
;;; debug-keys.el ends here
