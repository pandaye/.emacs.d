;;; display-config-test.el --- Display environment split -*- lexical-binding: t; -*-

;;; Commentary:
;; 校验 `init-gui' / `init-tui' 的环境分工。
;;
;; ERT 跑在 `emacs -Q --batch' 下，也就是没有图形帧：所以加载时生效的
;; 一定是终端那一套，图形侧通过 mock `display-graphic-p' 验证。

;;; Code:

(require 'cl-lib)
(require 'ert)

(ert-deftest display-config-loads-both-environment-layers ()
  (should (featurep 'init-gui))
  (should (featurep 'init-tui))
  (should (memq #'pandaye/gui--apply-frame after-make-frame-functions))
  (should (memq #'pandaye/tui--apply-frame after-make-frame-functions))
  (should (memq #'pandaye/input-apply-rimel-candidate-display
                after-make-frame-functions)))

(ert-deftest display-config-applies-terminal-settings-in-batch ()
  (should (display-table-slot standard-display-table 'truncation))
  (should (display-table-slot standard-display-table 'wrap))
  (should-not (default-value 'auto-composition-mode))
  (should (member interprogram-cut-function
                  '(terminal-clipboard-osc-52-cut-function
                    terminal-clipboard-pbcopy))))

(ert-deftest display-config-tui-layer-applies-on-terminal-frames ()
  ;; 模拟 daemon 场景：加载时没应用终端设定，靠帧 hook 补上。
  (let ((cut interprogram-cut-function)
        (paste interprogram-paste-function))
    (unwind-protect
        (progn
          (should (pandaye/tui--apply-frame (selected-frame)))
          (should (eq interprogram-cut-function
                      #'terminal-clipboard-osc-52-cut-function)))
      (setq interprogram-cut-function cut
            interprogram-paste-function paste))))

(ert-deftest display-config-gui-layer-ignores-terminal-frames ()
  (let ((dialog use-dialog-box)
        (wheel mouse-wheel-scroll-amount))
    (should-not (pandaye/gui--apply-frame nil))
    (should (eq use-dialog-box dialog))
    (should (equal mouse-wheel-scroll-amount wheel))))

(ert-deftest display-config-gui-layer-applies-on-graphic-frames ()
  (let ((use-dialog-box t)
        (use-file-dialog t)
        (wheel mouse-wheel-scroll-amount)
        (fonts-applied nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'pandaye/gui--apply-fonts)
               (lambda (_frame) (setq fonts-applied t))))
      (unwind-protect
          (progn
            (should (pandaye/gui--apply-frame nil))
            (should fonts-applied)
            (should-not use-dialog-box)
            (should-not use-file-dialog)
            (should (equal mouse-wheel-scroll-amount
                           (list pandaye/gui-wheel-scroll-lines))))
        (setq mouse-wheel-scroll-amount wheel)))))

(ert-deftest display-config-cursor-color-follows-frame-type ()
  (let (captured sent)
    ;; 图形帧：写 `cursor-color' 帧参数，不发 OSC 序列
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame param value)
                 (setq captured (list frame param value))))
              ((symbol-function 'send-string-to-terminal)
               (lambda (string) (setq sent string))))
      (cursor-display-set-cursor-color "#123456"))
    (should (equal captured (list (selected-frame) 'cursor-color "#123456")))
    (should-not sent)
    ;; 终端帧：走 OSC 12
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil))
              ((symbol-function 'send-string-to-terminal)
               (lambda (string) (setq sent string))))
      (cursor-display-set-cursor-color "#abcdef"))
    (should (equal sent "\e]12;#abcdef\a"))))

(ert-deftest display-config-rimel-layout-follows-frame ()
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t)))
    (pandaye/input-apply-rimel-candidate-display nil)
    (should (eq rimel-posframe-style 'vertical)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil)))
    (pandaye/input-apply-rimel-candidate-display nil)
    (should (eq rimel-posframe-style 'horizontal))))

(ert-deftest display-config-focus-refresh-stays-callable ()
  "`after-focus-change-function' 是函数变量而不是 hook。
用 `add-hook' 挂载会把它的值变成列表，Emacs 一处理焦点变化就报
\"Invalid function\"；这里直接调用一次，确保接线方式没有回退。"
  (should (functionp after-focus-change-function))
  (should cursor-display--focus-refresh-installed)
  (let (failure)
    (condition-case err
        (funcall after-focus-change-function)
      (error (setq failure (error-message-string err))))
    (should-not failure)))

(provide 'display-config-test)
;;; display-config-test.el ends here
