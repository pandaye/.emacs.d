;;; init-gui.el --- Graphical display configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; 图形界面（pgtk / X11）专属配置；终端侧见 `init-tui'。
;;
;; 既要支持直接 `emacs' 启动图形界面，也要支持 `emacs --daemon' +
;; `emacsclient -c'：配置加载时若已经有图形帧就立即应用，否则挂到
;; `after-make-frame-functions'，等第一个图形帧出现时再应用。
;;
;; 所有设置都写成幂等的：同一个 Emacs 里可能出现多个图形帧，重复应用
;; 不会改变结果；终端帧进来时本模块整体 no-op。

;;; Code:

(require 'mwheel)

(defgroup pandaye/gui nil
  "图形界面显示设置。"
  :group 'faces)

(defcustom pandaye/gui-font-family "JetBrains Maple Mono"
  "图形界面默认面（`default' face）使用的等宽字体族。"
  :type 'string
  :group 'pandaye/gui)

(defcustom pandaye/gui-font-height 128
  "图形界面默认字号，单位是磅的百分之一（128 即 12.8pt）。
不用 12pt 是因为 96 DPI 下 12pt 对应 16px：ASCII 宽 9.6px 会四舍五入成
10px，而 CJK 宽 19.2px 会舍成 19px，中文每字差 1px，org 表格会歪。
12.8pt 对应 17px：ASCII 10px、CJK 20px，正好 2:1；13.5pt(18px)、
15pt(20px) 也保持 2:1，14pt(19px) 会差 1px。"
  :type 'integer
  :group 'pandaye/gui)

(defcustom pandaye/gui-cjk-font-family "JetBrains Maple Mono"
  "CJK 字符使用的字体族。"
  :type 'string
  :group 'pandaye/gui)

(defcustom pandaye/gui-cjk-font-rescale 1.0
  "CJK 字体相对默认字体的缩放比例。
中英文用同一族（JetBrains Maple Mono 本身覆盖 CJK）时保持 1.0，避免中文被放大。"
  :type 'number
  :group 'pandaye/gui)

(defcustom pandaye/gui-emoji-font-family "Noto Color Emoji"
  "彩色 emoji 使用的字体族；nil 表示沿用 Emacs 默认回退。"
  :type '(choice (const :tag "默认" nil) string)
  :group 'pandaye/gui)

(defcustom pandaye/gui-line-spacing 2
  "图形界面的额外行距（像素）；nil 表示不额外设置。"
  :type '(choice (const :tag "默认" nil) integer)
  :group 'pandaye/gui)

(defcustom pandaye/gui-wheel-scroll-lines 5
  "图形界面滚轮每格滚动的行数。"
  :type 'integer
  :group 'pandaye/gui)

(defun pandaye/gui--apply-fonts (frame)
  "Set FRAME's default face and the CJK/emoji fontsets."
  (when (fboundp 'set-face-attribute)
    (set-face-attribute 'default frame
                        :family pandaye/gui-font-family
                        :height pandaye/gui-font-height))
  (setq face-font-rescale-alist
        (list (cons pandaye/gui-cjk-font-family
                    pandaye/gui-cjk-font-rescale)))
  ;; 没有图形支持的构建（例如 --without-x）里 `set-fontset-font' 不存在。
  (when (fboundp 'set-fontset-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset
                        (font-spec :family pandaye/gui-cjk-font-family)))
    (when (and pandaye/gui-emoji-font-family
               (fboundp 'find-font)
               (find-font (font-spec :family pandaye/gui-emoji-font-family)))
      (set-fontset-font t 'emoji
                        (font-spec :family pandaye/gui-emoji-font-family)))))

(defun pandaye/gui--apply-globals ()
  "Apply GUI-only global settings.
Idempotent; a daemon serving several frames may call it repeatedly."
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  ;; 图形界面下弹出原生对话框会和 minibuffer 的习惯割裂。
  (setq use-dialog-box nil
        use-file-dialog nil
        frame-resize-pixelwise t
        mouse-wheel-scroll-amount (list pandaye/gui-wheel-scroll-lines))
  (when pandaye/gui-line-spacing
    (setq-default line-spacing pandaye/gui-line-spacing)))

(defun pandaye/gui--apply-frame (&optional frame)
  "Apply GUI display settings to FRAME.
FRAME defaults to the selected frame.  Terminal frames are ignored, so this
function is safe to run from `after-make-frame-functions'."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (pandaye/gui--apply-globals)
      (pandaye/gui--apply-fonts frame)
      t)))

(add-hook 'after-make-frame-functions #'pandaye/gui--apply-frame)

;; 普通 `emacs' 图形启动时初始帧已经存在，直接应用；daemon 下交给上面的 hook。
(when (display-graphic-p)
  (pandaye/gui--apply-frame (selected-frame)))

(provide 'init-gui)
;;; init-gui.el ends here
