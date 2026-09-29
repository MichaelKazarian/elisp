;;; rc-mouse.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  ;; Increase the number of lines scrolled per mouse wheel click
  (setq mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control) . nil)))
  ;; Enable progressive speed for faster scrolling when spinning the wheel quickly
  (setq mouse-wheel-progressive-speed t)
  )

(add-to-list 'load-path (expand-file-name "~/elisp/mode/mouse-scroll-restore/"))
(require 'mouse-scroll-restore)
(mouse-scroll-restore-mode t)
;;; rc-mouse.el ends here
