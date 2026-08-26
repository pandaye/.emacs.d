;;; ai-quota-test.el --- Tests for ai-quota helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'ai-quota)

(ert-deftest ai-quota-cny-converts-quota-to-yuan ()
  (should (= (ai-quota--cny 500000) 7.3))
  (should (= (ai-quota--cny 0) 0.0)))

(ert-deftest ai-quota-format-cny-uses-two-decimals ()
  (should (equal (ai-quota--format-cny 4630104) "¥67.60"))
  (should (equal (ai-quota--format-cny 0) "¥0.00")))

(ert-deftest ai-quota-format-int-adds-thousands-separators ()
  (should (equal (ai-quota--format-int 1234567) "1,234,567"))
  (should (equal (ai-quota--format-int 999) "999"))
  (should (equal (ai-quota--format-int 1000) "1,000"))
  (should (equal (ai-quota--format-int 0) "0")))

(ert-deftest ai-quota-format-tokens-uses-k-m-units ()
  (should (equal (ai-quota--format-tokens 45200000) "45.2M"))
  (should (equal (ai-quota--format-tokens 12300) "12.3K"))
  (should (equal (ai-quota--format-tokens 999) "999")))

(ert-deftest ai-quota-json-true-p-handles-json-booleans ()
  (should (ai-quota--json-true-p t))
  (should-not (ai-quota--json-true-p :json-false))
  (should-not (ai-quota--json-true-p nil)))

(ert-deftest ai-quota-info-data-extracts-data-object ()
  (should (equal (ai-quota--info-data '((success . t) (data (name . "x"))))
                 '((name . "x"))))
  (should-not (ai-quota--info-data nil)))

(ert-deftest ai-quota-log-items-extracts-items-list ()
  (should (equal (ai-quota--log-items '((data (items (a) (b)))))
                 '((a) (b))))
  (should (equal (ai-quota--log-items '((data (items)))) '())))

(ert-deftest ai-quota-decode-other-parses-json-string ()
  (should (equal (ai-quota--decode-other "{\"cache_tokens\":123}")
                 '((cache_tokens . 123))))
  (should-not (ai-quota--decode-other nil))
  (should-not (ai-quota--decode-other "not json")))

(ert-deftest ai-quota-aggregate-by-model-sums-tokens-and-quota ()
  (let ((items
         '(((model_name . "m1") (prompt_tokens . 100) (completion_tokens . 50)
            (quota . 150) (other . "{\"cache_tokens\":10}"))
           ((model_name . "m2") (prompt_tokens . 20) (completion_tokens . 10)
            (quota . 30) (other . nil))
           ((model_name . "m1") (prompt_tokens . 200) (completion_tokens . 100)
            (quota . 300) (other . "{\"cache_tokens\":20}")))))
    (let ((result (ai-quota--aggregate-by-model items)))
      (should (= (length result) 2))
      (should (equal (car (car result)) "m1"))
      (should (= (plist-get (cdr (car result)) :count) 2))
      (should (= (plist-get (cdr (car result)) :prompt) 300))
      (should (= (plist-get (cdr (car result)) :completion) 150))
      (should (= (plist-get (cdr (car result)) :quota) 450))
      (should (= (plist-get (cdr (car result)) :cache) 30))
      (should (= (plist-get (cdr (cadr result)) :count) 1)))))

(ert-deftest ai-quota-endpoint-builds-url-with-key-and-params ()
  (let ((ai-quota-api-key "sk-abc")
        (ai-quota-base-url "http://x:30080"))
    (should (equal (ai-quota--endpoint "api/token/info")
                   "http://x:30080/api/token/info?key=sk-abc"))
    (should (equal (ai-quota--endpoint "api/log/list"
                                       '(("page" . "2") ("pageSize" . "100")))
                   "http://x:30080/api/log/list?key=sk-abc&page=2&pageSize=100"))))

(ert-deftest ai-quota-summary-line-formats-balance ()
  (should
   (equal (ai-quota--summary-line
           '((name . "t") (remainQuota . 5000000)
             (unlimitedQuota . :json-false)
             (consumption (total . 1000000)) (expiredTime . -1)))
          "名称 t · 剩余 ¥73.00 · 已消费 ¥14.60 · 到期 永久"))
  (should (equal (ai-quota--summary-line nil) "加载失败")))

(ert-deftest ai-quota-summary-line-handles-unlimited ()
  (let ((line (ai-quota--summary-line
               '((name . "u") (remainQuota . 1000000000)
                 (unlimitedQuota . t)
                 (consumption (total . 0)) (expiredTime . 1787535531)))))
    (should (string-match-p "无限" line))))

(provide 'ai-quota-test)
;;; ai-quota-test.el ends here
