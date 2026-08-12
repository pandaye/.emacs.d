;; -*- lexical-binding: t; -*-
;;; org-tmux.el --- Deprecated compatibility shim for tmux manager

;; Author: Pandaye
;; Keywords: tmux, compatibility

;;; Commentary:
;; This module is deprecated.
;; Use `tmux-manager.el' and `tmux-manager' instead.

;;; Code:

(require 'tmux-manager)

(make-obsolete 'org-tmux-manager 'tmux-manager "2026-03-16")
(defalias 'org-tmux-manager #'tmux-manager)

(make-obsolete 'org-tmux-switch-to-buffer 'tmux-manager-switch-to-buffer "2026-03-16")
(defalias 'org-tmux-switch-to-buffer #'tmux-manager-switch-to-buffer)

(defvaralias 'org-tmux-manager-buffer-name 'tmux-manager-buffer-name)

(display-warning
 'org-tmux
 "org-tmux.el is deprecated. Please require `tmux-manager` instead."
 :warning)

(provide 'org-tmux)

;;; org-tmux.el ends here
