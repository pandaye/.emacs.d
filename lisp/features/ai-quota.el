;; -*- lexical-binding: t; -*-
;;; ai-quota.el --- AI 额度展板

;;; Commentary:
;; 查询 new-api 风格网关（令牌查询页面）上某个 API key 的额度与使用统计，
;; 以展板形式展示：额度卡片 + 按模型的聚合统计（次数 / token / 花费）。
;;
;; 网络访问走 curl 子进程 + SOCKS5 代理（Emacs 内建 url.el 不支持 socks5）。
;; 敏感信息（API key）放在 gitignored 的 local-vars.local.el 中。
;;
;; 入口命令：`ai-quota-dashboard'。

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)

(defgroup ai-quota nil
  "AI 额度数据展板。"
  :group 'convenience)

(defcustom ai-quota-base-url "http://192.168.161.73:30080"
  "查询网关的 base URL。"
  :type 'string
  :group 'ai-quota)

(defcustom ai-quota-api-key nil
  "要查询额度的 API key。

敏感信息，请设置在 local-vars.local.el 中：

  (setq ai-quota-api-key \"sk-...\")"
  :type 'string
  :group 'ai-quota)

(defcustom ai-quota-socks-proxy "127.0.0.1:8888"
  "访问网关使用的 SOCKS5 代理地址。"
  :type 'string
  :group 'ai-quota)

(defcustom ai-quota-cny-rate 7.3
  "new-api quota 换算人民币的汇率（500000 quota = $1）。"
  :type 'number
  :group 'ai-quota)

(defcustom ai-quota-max-logs 1000
  "统计口径：按时间从新到旧拉取的最多调用日志条数。"
  :type 'integer
  :group 'ai-quota)

(defcustom ai-quota-buffer-name "*AI 额度展板*"
  "展板 buffer 名称。"
  :type 'string
  :group 'ai-quota)

(defcustom ai-quota-curl-timeout 20
  "每次 curl 请求的超时秒数。"
  :type 'integer
  :group 'ai-quota)

(defconst ai-quota--log-page-size 100
  "日志接口单页条数（服务端上限）。")

(defvar ai-quota--curl-program "curl"
  "curl 可执行文件名。")

;; ------------------------------------------------------------
;; 数值换算与格式化
;; ------------------------------------------------------------

(defun ai-quota--cny (quota)
  "把 new-api 的 QUOTA 整数换算为人民币。"
  (/ (* quota ai-quota-cny-rate) 500000.0))

(defun ai-quota--format-cny (quota)
  "把 QUOTA 格式化为两位小数的金额字符串。"
  (format "¥%.2f" (ai-quota--cny quota)))

(defun ai-quota--format-int (n)
  "把整数 N 格式化为千分位字符串。"
  (let* ((s (number-to-string n))
         (len (length s))
         (out '()))
    (dotimes (i len)
      (push (aref s (- len 1 i)) out)
      (when (and (/= (1+ i) len) (= (% (1+ i) 3) 0))
        (push ?, out)))
    (apply #'string out)))

(defun ai-quota--format-tokens (n)
  "把 token 数量 N 格式化为易读的 K/M 字符串。"
  (cond ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
        ((>= n 1000) (format "%.1fK" (/ n 1000.0)))
        (t (number-to-string n))))

;; ------------------------------------------------------------
;; HTTP 请求
;; ------------------------------------------------------------

(defun ai-quota--endpoint (path &optional params)
  "构造 API 请求 URL。

PATH 是接口路径（如 \"api/token/info\"），PARAMS 是附加查询参数
的 alist，API key 会自动附加。"
  (concat ai-quota-base-url "/" path
          "?key=" (url-hexify-string ai-quota-api-key)
          (mapconcat (lambda (pair)
                       (format "&%s=%s"
                               (url-hexify-string (car pair))
                               (url-hexify-string (cdr pair))))
                     params "")))

(defun ai-quota--curl-async (url callback)
  "通过 SOCKS5 代理异步请求 URL，成功后以响应体字符串调用 CALLBACK。

失败时以 nil 调用 CALLBACK。"
  (unless (executable-find ai-quota--curl-program)
    (funcall callback nil)
    (user-error "未找到 curl：%s" ai-quota--curl-program))
  (let ((buffer (generate-new-buffer " *ai-quota-curl*")))
    (make-process
     :name "ai-quota-curl"
     :buffer buffer
     :command (list ai-quota--curl-program
                    "--silent" "--show-error"
                    "--max-time" (number-to-string ai-quota-curl-timeout)
                    "--socks5-hostname" ai-quota-socks-proxy
                    url)
     :sentinel
     (lambda (proc _event)
       (let ((status (process-status proc)))
         (when (memq status '(exit signal))
           (let ((failed (not (and (eq status 'exit)
                                   (zerop (process-exit-status proc))))))
             (with-current-buffer buffer
               (let ((body (buffer-string)))
                 (kill-buffer buffer)
                 (funcall callback (unless failed body)))))))))))

(defun ai-quota--json-decode (string)
  "把 STRING 解析为 JSON alist（数组为 list），失败返回 nil。"
  (when (and (stringp string) (string-search "{" string))
    (condition-case nil
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (let ((json-array-type 'list)
                (json-object-type 'alist)
                (json-key-type 'symbol))
            (json-read)))
      (error nil))))

(defun ai-quota--fetch-json (url callback)
  "请求 URL 并解析 JSON 后以结果调用 CALLBACK，失败传 nil。"
  (ai-quota--curl-async
   url
   (lambda (body)
     (funcall callback (ai-quota--json-decode body)))))

;; ------------------------------------------------------------
;; 数据解析与聚合
;; ------------------------------------------------------------

(defun ai-quota--info-data (info)
  "从 token/info 接口的解析结果 INFO 中提取 data 对象。"
  (and (consp info) (cdr (assq 'data info))))

(defun ai-quota--json-true-p (value)
  "判断 JSON 解析出的 VALUE 是否为 true。"
  (and value (not (eq value :json-false))))

(defun ai-quota--log-items (logs)
  "从解析后的 LOGS alist 提取调用记录列表。"
  (let ((data (assq 'data logs)))
    (or (and data (cdr (assq 'items data))) '())))

(defun ai-quota--decode-other (other)
  "把调用记录的 other 字段（JSON 字符串）解析为 alist，失败返回 nil。"
  (when (stringp other)
    (condition-case nil
        (with-temp-buffer
          (insert other)
          (goto-char (point-min))
          (let ((json-array-type 'list)
                (json-object-type 'alist)
                (json-key-type 'symbol))
            (json-read)))
      (error nil))))

(defun ai-quota--aggregate-by-model (items)
  "把调用记录 ITEMS 按模型聚合。

返回按消耗 quota 降序排列的列表，每项形如
  (model-name . (count N prompt N completion N quota N cache N))。"
  (let ((acc (make-hash-table :test #'equal)))
    (dolist (item items)
      (let* ((model (or (cdr (assq 'model_name item)) "unknown"))
             (s (gethash model acc)))
        (unless s
          (setq s (list :count 0 :prompt 0 :completion 0 :quota 0 :cache 0))
          (puthash model s acc))
        (cl-incf (plist-get s :count))
        (cl-incf (plist-get s :prompt) (or (cdr (assq 'prompt_tokens item)) 0))
        (cl-incf (plist-get s :completion) (or (cdr (assq 'completion_tokens item)) 0))
        (cl-incf (plist-get s :quota) (or (cdr (assq 'quota item)) 0))
        (let ((other (ai-quota--decode-other (cdr (assq 'other item)))))
          (cl-incf (plist-get s :cache) (or (cdr (assq 'cache_tokens other)) 0)))))
    (sort (mapcar (lambda (m) (cons m (gethash m acc)))
                  (hash-table-keys acc))
          (lambda (a b)
            (> (plist-get (cdr a) :quota) (plist-get (cdr b) :quota))))))

;; ------------------------------------------------------------
;; 展板渲染
;; ------------------------------------------------------------

(defun ai-quota--pad (s width)
  "把字符串 S 右补空格到 WIDTH 列。"
  (concat s (make-string (max 0 (- width (string-width s))) ? )))

(defun ai-quota--summary-line (data)
  "根据 token/info 的 DATA 对象生成额度摘要行。"
  (if (null data)
      "加载失败"
    (let* ((name (cdr (assq 'name data)))
           (unlimited (ai-quota--json-true-p
                       (cdr (assq 'unlimitedQuota data))))
           (remain (cdr (assq 'remainQuota data)))
           (consumed (cdr (assq 'total (assq 'consumption data))))
           (expired (cdr (assq 'expiredTime data)))
           (remain-str (if unlimited "无限" (ai-quota--format-cny remain)))
           (expire-str (if (and (integerp expired) (> expired 0))
                           (format-time-string "%Y-%m-%d" expired)
                         "永久")))
      (format "名称 %s · 剩余 %s · 已消费 %s · 到期 %s"
              (or name "?") remain-str
              (ai-quota--format-cny (or consumed 0)) expire-str))))

(defun ai-quota--model-line (entry max-quota)
  "把聚合结果 ENTRY 渲染为一行模型统计，含按花费比例绘制的条形。"
  (let* ((name (car entry))
         (s (cdr entry))
         (count (plist-get s :count))
         (tokens (+ (plist-get s :prompt) (plist-get s :completion)))
         (quota (plist-get s :quota))
         (bar-width 18)
         (filled (max 0 (min bar-width
                             (round (* bar-width
                                       (/ (float quota)
                                          (max 1 max-quota))))))))
    (concat (ai-quota--pad (format "%s" name) 26)
            (propertize (make-string filled ?█) 'face 'success)
            (ai-quota--pad "" (- bar-width filled))
            " "
            (ai-quota--pad (format "%s 次" (ai-quota--format-int count)) 10)
            (ai-quota--pad (ai-quota--format-tokens tokens) 9)
            (propertize (ai-quota--format-cny quota) 'face 'warning))))

(defun ai-quota--total-line (models)
  "生成 MODELS 的合计行。"
  (let* ((count (apply #'+ (mapcar (lambda (m) (plist-get (cdr m) :count)) models)))
         (tokens (apply #'+
                        (mapcar (lambda (m)
                                  (+ (plist-get (cdr m) :prompt)
                                     (plist-get (cdr m) :completion)))
                                models)))
         (quota (apply #'+ (mapcar (lambda (m) (plist-get (cdr m) :quota)) models))))
    (format "合计 %s 次 · %s token · %s"
            (ai-quota--format-int count)
            (ai-quota--format-tokens tokens)
            (ai-quota--format-cny quota))))

(defun ai-quota--render (data items)
  "在展板 buffer 中渲染额度 DATA 和调用日志 ITEMS 的聚合统计。"
  (let* ((w (max 40 (min 88 (window-width))))
         (inhibit-read-only t)
         (models (ai-quota--aggregate-by-model items))
         (max-quota (if models
                        (apply #'max
                               (mapcar (lambda (m) (plist-get (cdr m) :quota))
                                       models))
                      1))
         (row (lambda (s) (concat "│ " (ai-quota--pad s (- w 2)) " │"))))
    (erase-buffer)
    (insert (concat "┌" (make-string w ?─) "┐") "\n")
    (insert (funcall row (propertize "◆ AI 额度展板" 'face 'bold)) "\n")
    (insert (funcall row (ai-quota--summary-line data)) "\n")
    (insert (concat "├" (make-string w ?─) "┤") "\n")
    (insert (funcall row (format "模型统计 · 最近 %s 次调用"
                                 (ai-quota--format-int (length items)))) "\n")
    (dolist (m models)
      (insert (funcall row (ai-quota--model-line m max-quota)) "\n"))
    (when models
      (insert (funcall row (ai-quota--total-line models)) "\n"))
    (insert (concat "├" (make-string w ?─) "┤") "\n")
    (insert (funcall row (propertize "g 刷新 · q 退出" 'face 'shadow)) "\n")
    (insert (concat "└" (make-string w ?─) "┘") "\n")
    (goto-char (point-min))))

(defun ai-quota--render-loading ()
  "渲染刷新中的占位内容。"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "◆ AI 额度展板 — 刷新中…" 'face 'bold) "\n")))

;; ------------------------------------------------------------
;; 主模式与命令
;; ------------------------------------------------------------

(define-derived-mode ai-quota-mode special-mode "AI额度"
  "AI 额度展板主模式。")

(define-key ai-quota-mode-map (kbd "g") #'ai-quota-refresh)
(define-key ai-quota-mode-map (kbd "r") #'ai-quota-refresh)
(define-key ai-quota-mode-map (kbd "q") #'quit-window)

(defun ai-quota--fetch-pages (page total-pages acc callback)
  "依次拉取日志分页并累加到 ACC，完成后以总列表调用 CALLBACK。

页空或越界时提前结束。"
  (if (> page total-pages)
      (funcall callback acc)
    (ai-quota--fetch-json
     (ai-quota--endpoint "api/log/list"
                         (list (cons "page" (number-to-string page))
                               (cons "pageSize"
                                     (number-to-string ai-quota--log-page-size))))
     (lambda (logs)
       (let ((items (ai-quota--log-items logs)))
         (if (null items)
             (funcall callback acc)
           (ai-quota--fetch-pages (1+ page) total-pages
                                  (nconc acc items) callback)))))))

(defun ai-quota-refresh ()
  "刷新 AI 额度展板数据。"
  (interactive)
  (unless ai-quota-api-key
    (user-error "ai-quota-api-key 未设置，请在 local-vars.local.el 中配置"))
  (let* ((buffer (get-buffer-create ai-quota-buffer-name))
         (pages (max 1 (ceiling ai-quota-max-logs ai-quota--log-page-size))))
    (with-current-buffer buffer
      (ai-quota--render-loading))
    (ai-quota--fetch-json
     (ai-quota--endpoint "api/token/info")
     (lambda (info)
       (ai-quota--fetch-pages
        1 pages '()
        (lambda (items)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (ai-quota--render (ai-quota--info-data info) items)))))))))

(defun ai-quota-dashboard ()
  "打开 AI 额度展板。"
  (interactive)
  (let ((buffer (get-buffer-create ai-quota-buffer-name)))
    (pop-to-buffer buffer)
    (unless (derived-mode-p 'ai-quota-mode)
      (ai-quota-mode))
    (ai-quota-refresh)))

(provide 'ai-quota)
;;; ai-quota.el ends here
