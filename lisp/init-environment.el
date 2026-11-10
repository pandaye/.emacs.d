;;; init-environment.el --- Register the configuration layer directories -*- lexical-binding: t; -*-

;;; Commentary:
;; 三个层目录（core/、modules/、features/）的唯一登记处。
;;
;; 这个文件描述「层」本身，所以放在 `lisp/' 根而不是任何一个层里：
;; 入口 `init.el' 只需把 `lisp/' 挂到 `load-path' 并 `require' 它，
;; 之后 `require 'init-config' 就能找到各层模块。

;;; Code:

(dolist (directory '("core" "modules" "features"))
  (add-to-list 'load-path
               (expand-file-name directory
                                 (expand-file-name "lisp" user-emacs-directory))))

(provide 'init-environment)
;;; init-environment.el ends here
