;; Evil 模式配置 - Vim 编辑体验
(use-package evil
  :ensure t
  :init
  ;; 在 evil 加载前的配置
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  ;; 启用 evil 模式
  (evil-mode 1)
  
  ;; 在某些模式下使用 Emacs 状态
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Evil Collection - 为更多模式提供 evil 支持
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Undo Tree - 更好的撤销体验
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  ;; 禁用持久化撤销历史到文件
  (setq undo-tree-auto-save-history nil))

;; Evil Surround - 快速编辑包围字符
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Evil Commentary - 快速注释
(use-package evil-commentary
  :ensure t
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-escape
  :ensure t
  :after evil
  :init
  (setq evil-escape-key-sequence "jj"
	evil-escape-delay 0.2)
  :config
  ;; 只在 insert 状态下启用 evil-escape
  (defun my/evil-escape-insert-only ()
    (not (eq evil-state 'insert)))
  (setq evil-escape-inhibit-functions '(my/evil-escape-insert-only))
  (evil-escape-mode 1))

;; Evil 状态颜色自定义 - 更加显眼
(with-eval-after-load 'doom-modeline
  ;; 启用完整的 Evil 状态文本显示
  (setq doom-modeline-modal-state-icon nil)         ; 禁用图标
  (setq doom-modeline-modal-modern-icon nil)        ; 禁用现代图标
  
  ;; 自定义 Evil 状态显示函数
  (defun my-doom-modeline-evil-state ()
    "Return current Evil state with full text."
    (when (bound-and-true-p evil-local-mode)
      (let ((tag (cond
                  ((eq evil-state 'normal) " NORMAL ")
                  ((eq evil-state 'insert) " INSERT ")
                  ((eq evil-state 'visual) " VISUAL ")
                  ((eq evil-state 'replace) " REPLACE ")
                  ((eq evil-state 'motion) " MOTION ")
                  ((eq evil-state 'operator) " OPERATOR ")
                  ((eq evil-state 'emacs) " EMACS ")
                  (t " ??? "))))
        (propertize tag 'face
                    (cond
                     ((eq evil-state 'normal) 'doom-modeline-evil-normal-state)
                     ((eq evil-state 'insert) 'doom-modeline-evil-insert-state)
                     ((eq evil-state 'visual) 'doom-modeline-evil-visual-state)
                     ((eq evil-state 'replace) 'doom-modeline-evil-replace-state)
                     ((eq evil-state 'motion) 'doom-modeline-evil-motion-state)
                     ((eq evil-state 'operator) 'doom-modeline-evil-operator-state)
                     ((eq evil-state 'emacs) 'doom-modeline-evil-emacs-state)
                     (t 'doom-modeline-evil-normal-state))))))
  
  ;; 重写 doom-modeline 的 modal segment
  (doom-modeline-def-segment my-modal
    "The modal editing state indicator for Evil and Overwrite mode."
    (my-doom-modeline-evil-state))
  
  ;; 定义 Evil 状态颜色配置表
  (defvar my-doom-evil-state-faces
    '((doom-modeline-evil-normal-state   "#458588" "#ebdbb2")
      (doom-modeline-evil-insert-state   "#d79921" "#282828")
      (doom-modeline-evil-visual-state   "#98971a" "#282828")
      (doom-modeline-evil-replace-state  "#cc241d" "#ebdbb2")
      (doom-modeline-evil-motion-state   "#8ec07c" "#282828")
      (doom-modeline-evil-emacs-state    "#689d6a" "#282828")
      (doom-modeline-evil-operator-state "#b16286" "#ebdbb2"))
    "Evil state face configurations: (face-name background foreground)")

  ;; 批量应用 Evil 状态颜色
  (dolist (config my-doom-evil-state-faces)
    (let ((face (nth 0 config))
          (bg (nth 1 config))
          (fg (nth 2 config)))
      (when (facep face)  ; 确保 face 存在再设置
        (set-face-attribute face nil
                            :background bg
                            :foreground fg
                            :weight 'bold
                            :box `(:line-width 2 :color ,bg)))))

  ;; 自定义模式行布局 - 使用完整文本的 Evil 状态
  (doom-modeline-def-modeline 'main
    '(bar my-modal workspace-name window-number buffer-info remote-host buffer-position selection-info)
    '(misc-info time major-mode process vcs)))

(provide 'custom-evil)