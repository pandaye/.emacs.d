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
(global-set-key (kbd "C-c C-r") #'vertico-repeat)
(global-set-key (kbd "C-c b b") #'my/consult-buffer)
(global-set-key (kbd "C-x B") #'consult-buffer-other-window)
(global-set-key (kbd "C-c f g") #'consult-git-files)
(global-set-key (kbd "C-c f G") #'consult-git-grep)
(global-set-key (kbd "C-c f f") #'consult-find)
(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "C-r") #'consult-line)
(global-set-key (kbd "M-y") #'consult-yank-pop)

(global-set-key (kbd "C-c o g") #'gtd)
(global-set-key (kbd "C-c o t") #'open-today-diary)
(global-set-key (kbd "C-c o d") #'open-diary-by-date)
(global-set-key (kbd "C-c o l") #'list-diary-files)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c l c") #'org-capture)
(global-set-key (kbd "C-c l l") #'org-store-link)
(global-set-key (kbd "C-c l r") #'org-clock-report)
(global-set-key (kbd "C-c n l") #'my/org-roam-list-notes-by-mtime)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n o") #'org-roam-buffer-toggle)

(global-set-key (kbd "C-c i i") #'toggle-input-method)
(global-set-key (kbd "C-c i j") #'my/set-rime-jp)
(global-set-key (kbd "C-c i f") #'my/set-rime-zh)
(global-set-key (kbd "C-c .") #'my/rimel-toggle-ascii-punct)
(global-set-key (kbd "C-.") #'my/rimel-toggle-ascii-punct)
(global-set-key (kbd "C-c d d") #'tmux-manager-switch-to-buffer)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
