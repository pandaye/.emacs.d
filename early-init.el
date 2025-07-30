;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;-------------------------Performance Optimization--------------------------
;; Defer garbage collection during startup for better performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore garbage collection settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Temporarily disable file-name-handler-alist for faster startup
(defvar pandaye--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist pandaye--file-name-handler-alist)))

;;-------------------------Package Management---------------------------------
;; Prevent package.el from loading packages before init.el
(setq package-enable-at-startup nil)
;; 不需要 package-quickstart，会与 use-package 冲突

;;-------------------------UI Optimization------------------------------------
;; Disable startup screen and messages
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by setting these early
(setq frame-inhibit-implied-resize t)

;;-------------------------CLI vs GUI Settings-------------------------------
(if (display-graphic-p)
    ;; GUI settings
    (progn
      (push '(font . "Fira Mono-13") default-frame-alist)
      (push '(left-fringe . 3) default-frame-alist)
      (push '(right-fringe . 0) default-frame-alist)
      (push '(internal-border-width . 5) default-frame-alist)
      (push '(width . 140) default-frame-alist)
      (push '(height . 40) default-frame-alist))  ; 修正：height 而不是 length
  ;; CLI settings - 在终端中不设置字体和窗口大小
  (progn
    (setq menu-bar-mode nil)
    (setq tool-bar-mode nil)))

;;-------------------------Additional Optimizations-------------------------
;; Improve redisplay performance
(setq auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; Reduce rendering workload by not rendering cursors or regions in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions
(setq jit-lock-defer-time 0)