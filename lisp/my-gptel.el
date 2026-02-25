;; -*- lexical-binding: t; -*-
;;; my-gptel.el --- gptel & gptel-agent configuration

;; gptel: LLM client for Emacs
;; Backend: GitHub Copilot (OAuth, no API key needed)
;; First use: M-x gptel-gh-login  (opens browser for device authorization)
(use-package gptel
  :config
  ;; 注册 GitHub Copilot 后端并设为默认
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-model   'claude-sonnet-4.5)
  :bind
  (("C-c v s" . gptel-send)
   ("C-c v m" . gptel-menu)
   ("C-c v v" . gptel)))

;; gptel-agent: 为 gptel 添加工具调用和 agentic 能力
;; 工具包括：web 搜索、URL 抓取、文件读写、Shell 命令等
;; 默认只有 web 搜索 / URL 读取 / 文件读取自动执行，其他操作需确认
(use-package gptel-agent
  :after gptel
  :config
  ;; 加载 agent 工具和 preset（必须调用）
  (gptel-agent-update))

(provide 'my-gptel)
;;; my-gptel.el ends here
