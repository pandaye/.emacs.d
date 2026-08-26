# 喝水提醒（hydration）实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 新增 `lisp/features/hydration.el`：定时提醒喝水，到点把 tmux 窗口 0 改名为「💧 要喝水」、发 echo 消息、模式行显示 💧 标记；用户 `M-x hydration-confirm` 确认后恢复原窗口名并重新计时。

**架构：** 纯 feature（自包含领域库），复用 `tmux-manager` 的 tmux 辅助函数（`tmux-manager--run-tmux-command` / `tmux-manager--tmux-available-p` / `tmux-manager--get-current-session`），通过 `cl-letf` mock 这些函数做单测。模式行指示器追加到 `mode-line-format`（init-ui 先加载，故在 hydration 加载时其默认值已就绪）。定时器用一次性 `run-at-time`，confirm 时取消并重挂下一轮。

**技术栈：** Emacs Lisp（`run-at-time`、`tmux-manager` feature、`cl-lib`、ERT）。

**与设计文档的偏差（已确认）：** 设计文档写"在 `lisp/init-config.el` 加 `(require 'hydration)`"，但实际架构中 feature 一律由 module 加载（如 `init-org.el` 加载 `tmux-manager`）。本计划改为在 `lisp/modules/init-editor.el` 的「基础工具」区加载 `hydration`。

---

### 任务 1：编写失败的测试（hydration-test.el + 接入 run-tests）

**文件：**
- 创建：`tests/hydration-test.el`
- 修改：`tests/run-tests.el:3-11`（追加一行 load）

- [ ] **步骤 1：创建 `tests/hydration-test.el`**

```elisp
;;; hydration-test.el --- Tests for hydration reminder -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'hydration)

(ert-deftest hydration-start-schedules-timer-and-saves-window-name ()
  (let ((hydration--timer nil) (hydration--active nil) (hydration--original-name nil))
    (cl-letf (((symbol-function 'tmux-manager--tmux-available-p) (lambda () t))
              ((symbol-function 'tmux-manager--get-current-session) (lambda () "main"))
              ((symbol-function 'tmux-manager--run-tmux-command)
               (lambda (&rest args)
                 (if (member "display-message" args) "work" nil))))
      (hydration-start)
      (should (timerp hydration--timer))
      (should (equal hydration--original-name "work"))
      (should-not hydration--active)
      (when (timerp hydration--timer) (cancel-timer hydration--timer)))))

(ert-deftest hydration-fire-renames-window-and-activates ()
  (let ((hydration--original-name "work") (renames nil))
    (cl-letf (((symbol-function 'tmux-manager--tmux-available-p) (lambda () t))
              ((symbol-function 'tmux-manager--get-current-session) (lambda () "main"))
              ((symbol-function 'tmux-manager--run-tmux-command)
               (lambda (&rest args) (push args renames) nil)))
      (hydration--fire)
      (should hydration--active)
      (should (member '("rename-window" "-t" "main:0" "💧 要喝水") renames)))))

(ert-deftest hydration-confirm-restores-name-and-rearms ()
  (let ((hydration--original-name "work") (hydration--active t) (renames nil))
    (cl-letf (((symbol-function 'tmux-manager--tmux-available-p) (lambda () t))
              ((symbol-function 'tmux-manager--get-current-session) (lambda () "main"))
              ((symbol-function 'tmux-manager--run-tmux-command)
               (lambda (&rest args) (push args renames) nil)))
      (hydration-confirm)
      (should-not hydration--active)
      (should (timerp hydration--timer))
      (should (member '("rename-window" "-t" "main:0" "work") renames))
      (when (timerp hydration--timer) (cancel-timer hydration--timer)))))

(ert-deftest hydration-confirm-when-inactive-is-noop ()
  (let ((hydration--active nil) (renames nil))
    (cl-letf (((symbol-function 'tmux-manager--run-tmux-command)
               (lambda (&rest args) (push args renames) nil)))
      (hydration-confirm)
      (should-not renames)
      (should-not hydration--active))))

(ert-deftest hydration-fire-without-tmux-skips-rename ()
  (let ((hydration--original-name nil) (renames nil))
    (cl-letf (((symbol-function 'tmux-manager--tmux-available-p) (lambda () nil))
              ((symbol-function 'tmux-manager--run-tmux-command)
               (lambda (&rest args) (push args renames) nil)))
      (hydration--fire)
      (should hydration--active)
      (should-not renames))))

(ert-deftest hydration-cancel-restores-name-and-clears-timer ()
  (let ((hydration--original-name "work") (hydration--active t)
        (hydration--timer nil) (renames nil))
    (cl-letf (((symbol-function 'tmux-manager--tmux-available-p) (lambda () t))
              ((symbol-function 'tmux-manager--get-current-session) (lambda () "main"))
              ((symbol-function 'tmux-manager--run-tmux-command)
               (lambda (&rest args) (push args renames) nil)))
      (setq hydration--timer (run-at-time 100 nil #'hydration--fire))
      (hydration-cancel)
      (should-not hydration--active)
      (should-not (timerp hydration--timer))
      (should (member '("rename-window" "-t" "main:0" "work") renames)))))

(ert-deftest hydration-mode-line-indicator-reflects-state ()
  (let ((hydration--active t))
    (should (equal (hydration--mode-line-indicator) " 💧 ")))
  (let ((hydration--active nil))
    (should (equal (hydration--mode-line-indicator) ""))))

(provide 'hydration-test)
;;; hydration-test.el ends here
```

- [ ] **步骤 2：修改 `tests/run-tests.el` 追加加载**

在 `(load ... "ai-quota-test.el")` 后追加：

```elisp
(load (expand-file-name "hydration-test.el" (file-name-directory load-file-name)))
```

- [ ] **步骤 3：运行测试确认失败**

运行：`make test`
预期：FAIL / 报错，`Cannot open load file ... hydration`（feature 尚未实现），或 hydration-test 全部 FAIL。

- [ ] **步骤 4：Commit（红）**

```bash
git add tests/hydration-test.el tests/run-tests.el
git commit -m "test: 新增喝水提醒 hydration 测试骨架（红）"
```

---

### 任务 2：实现 `lisp/features/hydration.el`

**文件：**
- 创建：`lisp/features/hydration.el`

- [ ] **步骤 1：创建 `lisp/features/hydration.el`**

```elisp
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

(require 'cl-lib)
(require 'subr-x)
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
  (setq hydration--active t)
  (let ((session (hydration--session)))
    (when (and session hydration--original-name)
      (hydration--rename-window-0 session hydration-reminder-name)))
  (force-mode-line-update)
  (message "💧 该喝水啦！喝完后执行 M-x hydration-confirm"))

(defun hydration-confirm ()
  "Confirm the water was drunk, restore window name and re-arm the cycle."
  (interactive)
  (unless hydration--active
    (message "当前没有待确认的喝水提醒")
    (cl-return-from hydration-confirm))
  (let ((session (hydration--session)))
    (when (and session hydration--original-name)
      (hydration--rename-window-0 session hydration--original-name)))
  (setq hydration--active nil)
  (when (timerp hydration--timer)
    (cancel-timer hydration--timer))
  (hydration--arm-timer)
  (force-mode-line-update)
  (message "已确认喝水，下一轮提醒将在 %d 分钟后" hydration-interval))

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
```

- [ ] **步骤 2：运行测试确认通过**

运行：`make test`
预期：hydration-test 全部 PASS，原有测试不回归。

- [ ] **步骤 3：运行字节编译确认无错**

运行：`make compile`
预期：编译成功，无 error（hydration.el 无 warning 级 error）。

- [ ] **步骤 4：Commit（绿）**

```bash
git add lisp/features/hydration.el
git commit -m "feat: 新增喝水提醒 hydration feature"
```

---

### 任务 3：接入配置（init-editor.el 加载 feature）

**文件：**
- 修改：`lisp/modules/init-editor.el:87-91`（「基础工具」区，`network-tools` require 之后）

- [ ] **步骤 1：在 `lisp/modules/init-editor.el` 加入 require**

在 `(require 'network-tools)` 之后追加：

```elisp
(require 'hydration)
```

- [ ] **步骤 2：运行完整测试确认架构约束通过**

运行：`make test`
预期：`architecture-features-have-no-configuration-dependencies`、`architecture-feature-matches-file-name` 等全部 PASS，hydration-test PASS，无回归。

- [ ] **步骤 3：Commit**

```bash
git add lisp/modules/init-editor.el
git commit -m "feat: 接入 hydration 喝水提醒到编辑器配置"
```

---

### 任务 4：更新设计文档的接入描述

**文件：**
- 修改：`docs/superpowers/specs/2026-08-26-hydration-reminder-design.md`

- [ ] **步骤 1：修正接入章节**

将「接入」一节的内容改为：

```markdown
## 接入

- `lisp/modules/init-editor.el`：「基础工具」区 `(require 'hydration)`。
- 无全局键绑定（用户要求先不用快捷键）。
```

（feature 由 module 加载是既有架构约定，非 init-config 直连。）

- [ ] **步骤 2：Commit**

```bash
git add docs/superpowers/specs/2026-08-26-hydration-reminder-design.md
git commit -m "docs: 修正 hydration 设计文档接入描述"
```

---

## 验收清单

- [ ] `make test` 全绿（含新 hydration-test + architecture/config-contract 约束）
- [ ] `make compile` 无错误
- [ ] `M-x hydration-start` 启动周期；`M-x hydration-confirm` 恢复窗口名并重挂定时器；`M-x hydration-cancel` 取消并恢复
- [ ] 无 `my-*` 新符号；feature 未使用 `use-package` / `global-set-key` / `with-eval-after-load` / `(require 'init-…)`
