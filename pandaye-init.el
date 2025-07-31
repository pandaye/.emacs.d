(add-to-list 'exec-path "/opt/homebrew/bin/")
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(when (display-graphic-p)
  ;; GUI 专用配置
  (load "gui.el"))

;; Default Font
; (set-face-attribute 'default nil :font "Fira Mono 13")

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; 安装 diminish 以支持 :diminish 关键字
(use-package diminish
  :ensure t)

;; 备份文件配置 - 禁用 ~ 后缀文件，保留自动保存
(setq make-backup-files nil)       ; 禁用 file~ 备份文件


;; Ivy 配置 - 优化导航体验
(use-package ivy
  :ensure t
  :diminish
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t
        ivy-wrap t
        ivy-height 15
        ;; 智能模糊匹配 - 更精确的匹配策略
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)           ; M-x 使用模糊匹配
                                (counsel-find-file . ivy--regex-plus)       ; 文件查找使用增强匹配
                                (counsel-file-jump . ivy--regex-fuzzy)      ; 文件跳转使用模糊匹配
                                (swiper . ivy--regex-plus)                  ; 搜索使用增强匹配
                                (ivy-switch-buffer . ivy--regex-plus)       ; 缓冲区切换使用增强匹配
                                (t . ivy--regex-plus))                      ; 其他情况使用增强匹配
        ;; 忽略大小写
        ivy-case-fold-search-default t
        ;; 初始输入为空
        ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :init
  (counsel-mode 1)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ;; 递归查找文件（支持深度搜索）
   ("C-c f" . counsel-file-jump)
   ;; 在 Git 仓库中查找文件
   ("C-c G" . counsel-git-grep)))

(use-package swiper
  :ensure t
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper-backward)))

(use-package ivy-rich
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1)
  ;; 为 counsel-find-file 提供更丰富的信息显示
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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


(setq org-todo-keywords
      '((sequence "TODO(p!)" "PROCESSING(t!)" "BLOCK(s!)" "|" "DONE(d!)" "CANCEL(a@/!)")))

;; 设置任务样式
(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#66cccc"    :weight bold))
        ("BLOCK" . (:foreground "red"    :weight bold))
        ("PROCESSING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green"  :weight bold))
        ("CANCEL" . (:foreground "black"  :weight bold))
        ))

(setq gtd-path (expand-file-name "~/.pandaye-journal"))
(defvar org-gtd-file
  (concat gtd-path "/project.org"))

(defun gtd ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))

;; 设置 Org Agenda 快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
;; 将 GTD 快捷键改为 C-c o g，避免与 counsel-git 冲突
(global-set-key (kbd "C-c o g") 'gtd)

;; 加入到日程列表里
(setq org-agenda-files (list org-gtd-file))

;; org SSH 配置
(require 'org-ssh)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

;; 安装字体支持（可选，主要用于 GUI）
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Doom Modeline - 专为 CLI 优化的配置
(use-package doom-modeline
  :ensure t
  :init 
  (doom-modeline-mode 1)
  :config
  ;; CLI 优化的基本配置
  (setq doom-modeline-height 28)                    ; 增加高度以突出显示
  (setq doom-modeline-bar-width 4)                  ; 加宽左侧状态条
  (setq doom-modeline-window-width-limit 85)        ; 适合终端宽度
  
  ;; CLI 环境优化
  (setq doom-modeline-icon nil)                     ; 关闭图标（CLI 中可能显示异常）
  (setq doom-modeline-major-mode-icon nil)          ; 关闭主模式图标
  (setq doom-modeline-major-mode-color-icon nil)    ; 关闭彩色图标
  (setq doom-modeline-buffer-state-icon nil)        ; 关闭缓冲区状态图标
  (setq doom-modeline-buffer-modification-icon nil) ; 关闭修改状态图标
  (setq doom-modeline-unicode-fallback t)           ; 使用 Unicode 替代图标
  
  ;; Evil 状态突出显示
  (setq doom-modeline-modal t)                      ; 启用模态编辑指示
  (setq doom-modeline-modal-icon nil)               ; 关闭模态图标，使用文字
  (setq doom-modeline-modal-modern-icon nil)        ; 关闭现代图标样式
  
  ;; CLI 专用设置 - 显示完整状态名称
  (setq doom-modeline-always-show-macro-register t) ; 总是显示宏寄存器
  (setq doom-modeline-persp-name t)                 ; 显示透视图名称
  (setq doom-modeline-display-default-persp-name nil) ; 不显示默认透视图
  (setq doom-modeline-workspace-name t)             ; 显示工作区名称
  
  ;; 显示配置优化
  (setq doom-modeline-project-detection 'auto)      ; 自动检测项目
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project) ; 显示项目相对路径
  (setq doom-modeline-minor-modes nil)              ; 隐藏次要模式
  
  ;; Git 配置
  (setq doom-modeline-vcs-max-length 15)            ; 增加 Git 分支名显示长度
  (setq doom-modeline-check-simple-format t)        ; 简化的检查信息格式
  
  ;; 其他功能
  (setq doom-modeline-env-version nil)              ; 隐藏环境版本（减少杂乱）
  (setq doom-modeline-enable-word-count nil)        ; 不显示字数统计
  (setq doom-modeline-buffer-encoding t)            ; 隐藏编码信息
  (setq doom-modeline-indent-info nil)              ; 不显示缩进信息
  (setq doom-modeline-checker-simple-format t)      ; 简化的语法检查格式
  (setq doom-modeline-lsp nil)                      ; 关闭 LSP 显示
  (setq doom-modeline-github nil)                   ; 关闭 GitHub 通知
  (setq doom-modeline-mu4e nil)                     ; 关闭邮件显示
  (setq doom-modeline-irc nil)                      ; 关闭 IRC 显示
  
  ;; 时间显示（CLI 中有用）
  (setq doom-modeline-time t)                       ; 显示时间
  (setq doom-modeline-time-icon nil))               ; 时间不显示图标

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

