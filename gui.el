;; 基础增强
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(use-package powerline
  :config
  (powerline-default-theme))

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Sarasa Mono SC")))
(setq face-font-rescale-alist '(("Sarasa Mono SC" . 1.2)))