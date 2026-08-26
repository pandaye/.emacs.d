# 喝水提醒（hydration）设计

日期：2026-08-26
状态：已批准（等待规格审查）

## 目标

在 TUI 工作流中提醒用户定时喝水：到点把 tmux 窗口 0 改名为「💧 要喝水」，同时发 echo area 消息并在模式行加标记；喝完后用户手动确认，恢复原窗口名并重启下一轮周期。不记录日志，保持最简。

## 方案要点（已与用户确认）

- **提醒渠道**：tmux 窗口 0 改名 + echo area 消息 + 模式行 💧 标记（多通道兜底，避免只改名的盲区）。
- **周期**：固定间隔（默认 45 分钟，可配置），喝完后确认即重新计时。
- **不记录**任何喝水历史。
- **无快捷键**：`hydration-start` / `hydration-confirm` 为 `M-x` 可调用的交互命令，不绑全局键。

## 组件

### `lisp/features/hydration.el`（新增，自包含领域库）

遵循架构约束：不使用 `use-package`、`global-set-key`、`with-eval-after-load`、`(require 'init-…)`；文件末尾 `(provide 'hydration)`。

状态变量（`defvar` / `defvar-local`）：

- `hydration-interval`（`defcustom`，类型 `integer`，默认 45，单位分钟）
- `hydration--timer` — 当前 `run-at-time` 定时器对象，nil 表示未运行
- `hydration--original-name` — 保存的 tmux 窗口 0 原名，nil 表示尚未改名
- `hydration--active` — 是否处于「待喝水」状态

命令与函数：

- `hydration-start`（交互）— 启动周期。读取当前 tmux session 窗口 0 原名存入 `hydration--original-name`（不在 tmux 环境则置 nil），置 `hydration--active` 为 nil，启动 `run-at-time`（repeat 模式，间隔 `(* hydration-interval 60)` 秒）触发 `hydration--fire`。
- `hydration--fire` — 定时器回调。置 `hydration--active` 为 t；若 `hydration--original-name` 非 nil 则用 tmux 命令把窗口 0 改名为「💧 要喝水」（沿用 `tmux-manager--run-tmux-command`）；发 echo area 消息「💧 该喝水啦」；设置模式行标记（`mode-line-format` 或 `mode-line-process` 前插入标记，可用 `auto-insert` 式 hook 或直接向 `mode-line-misc-info` 添加，选择最小侵入方式）。
- `hydration-confirm`（交互）— 用户喝完后确认。若 `hydration--active` 为 nil，`message`「当前没有待确认的喝水提醒」并返回；若 `hydration--original-name` 非 nil 且 tmux 可用，把窗口 0 改名回 `hydration--original-name`；清模式行标记；置 `hydration--active` 为 nil；直接以相同间隔重启 `run-at-time` 定时器（不重新读取窗口名，保持 `hydration--original-name` 原值，下轮到点恢复同一名字）。
- `hydration-cancel`（交互）— 取消整个周期：取消定时器、恢复窗口名、清标记、状态复位。

边界处理：

- 不在 tmux 环境（`tmux` 命令不可用或未运行会话）：`hydration--original-name` 为 nil，跳过改名，只发消息 + 模式行标记。
- 窗口名被用户手动改过：仅当 `hydration--original-name` 非 nil（本功能保存过原名）才恢复，避免覆盖用户的手动命名。
- 定时器回调里 tmux 命令失败（如会话已退出）：`ignore-errors` 包裹，不中断提醒消息与模式行。

命名：feature 库前缀 `hydration-`；不留 `my-*` 别名（无历史兼容需求）。

### `lisp/modules/init-editor.el`（修改）

在「基础工具」区的 `(require 'network-tools)` 之后加入 `(require 'hydration)`（feature 由 module 加载，见「接入」章节注）。

### 测试：`tests/hydration-test.el`（新增）

- mock tmux：`cl-letf` / 动态绑定替换 `tmux-manager--run-tmux-command`，断言窗口改名调用序列与恢复调用序列。
- 定时器：调用 `hydration-start` 后断言 `hydration--timer` 非 nil；用 `run-at-time` 间隔设为极小值（如 0.01s）驱动 `hydration--fire` 执行，断言 `hydration--active` 为 t 且改名命令被调用。
- confirm 流程：断言恢复原名命令被调用、标记清除、新定时器已挂载。
- 非 tmux 环境：让 tmux 可用性判断返回 nil，断言只发消息不改名。
- 边界：`hydration-confirm` 在未提醒时调用返回提示而非报错。

## 接入

- `lisp/modules/init-editor.el`：「基础工具」区 `(require 'hydration)`。
- 无全局键绑定（用户要求先不用快捷键）。

> 注：feature 由 module 加载是既有架构约定（如 `init-org.el` 加载 `tmux-manager`），非 init-config 直连。

## 非目标（YAGNI）

- 不记录喝水日志/历史。
- 不做固定时刻提醒、不做跳过本次、不做桌面系统通知。
- 不自动检测「真的喝了水」，仅手动确认。
