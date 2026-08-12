;;; init-keybindings.el --- Global keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Becomes the sole owner of global bindings as domains are migrated.

;;; Code:

(global-set-key (kbd "C-c f s") #'save-buffer)
(global-set-key (kbd "C-c f r") #'projectile-ripgrep)
(global-set-key (kbd "C-c f d") #'my/common-dirs-find-file)
(global-set-key (kbd "C-c f p") #'projectile-find-file)

(global-set-key (kbd "C-c w o") #'ace-window)
(global-set-key (kbd "C-c w w") #'delete-other-windows)
(global-set-key (kbd "C-c w 2") #'split-window-below)
(global-set-key (kbd "C-c w 3") #'split-window-right)
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w k") #'windmove-up)
(global-set-key (kbd "C-c w q") #'delete-window)

(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-c b p") #'projectile-ibuffer)

(global-set-key (kbd "M-o") #'my/hyperbole-action-key)
(global-set-key [remap other-window] #'ace-window)
(global-set-key (kbd "C-c e h") #'hyperbole)
(global-set-key (kbd "C-c e a") #'hkey-either)
(global-set-key (kbd "C-c e ?") #'hkey-help)
(global-set-key (kbd "C-c t p") #'my/dired-project-root)
(global-set-key (kbd "C-c t t") #'my/dired-project-root)
(global-set-key (kbd "C-c j s") #'magit-status)
(global-set-key (kbd "C-c j p") #'magit-dispatch)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
