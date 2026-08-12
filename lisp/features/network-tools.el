;;; utils.el --- Utility functions for proxy and network -*- lexical-binding: t; -*-

;;; Commentary:
;; 代理开关和 IP 检测工具函数。

;;; Code:

(defcustom pandaye-proxy-services
  '(("http" . "127.0.0.1:20171")
    ("https" . "127.0.0.1:20171"))
  "代理服务器配置，键为协议类型，值为地址。"
  :type '(alist :key-type string :value-type string)
  :group 'convenience)

(defcustom pandaye-ip-test-url "https://api.ipify.org"
  "用于检测代理出口 IP 的 URL。"
  :type 'string
  :group 'convenience)

(defun proxy-on ()
  "开启代理"
  (interactive)
  (setq url-proxy-services pandaye-proxy-services)
  (message "代理已开启"))

(defun proxy-off ()
  "关闭代理"
  (interactive)
  (setq url-proxy-services nil)
  (message "代理已关闭"))

(defun test-proxy-ip ()
  "在 Emacs 里通过代理获取当前出口 IP。"
  (interactive)
  (url-retrieve
   pandaye-ip-test-url
   (lambda (status)
     (goto-char (point-min))
     (re-search-forward "\n\n")
     (message "Emacs 代理出口 IP: %s"
              (buffer-substring (point) (point-max))))))

(provide 'network-tools)
;;; utils.el ends here
