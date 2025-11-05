(defun proxy-on ()
  "开启代理"
  (interactive)
  (setq url-proxy-services
        '(("http" . "127.0.0.1:20171")
          ("https" . "127.0.0.1:20171")))
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
   "https://api.ipify.org"
   (lambda (status)
     (goto-char (point-min))
     (re-search-forward "\n\n")
     (message "Emacs 代理出口 IP: %s"
              (buffer-substring (point) (point-max))))))
