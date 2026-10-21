;;; screen.el --- General screen settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(setq inhibit-startup-message t
      scroll-conservatively 101         ; never recenter when scrolling
      scroll-preserve-screen-position t
      scroll-margin 1
      mouse-yank-at-point t ;middle-click pastes at cursor, not at click position
      display-time-string-forms '((format-time-string "%b.%d,%a %H:%M")))

(column-number-mode 1)
(display-time-mode 1)
(blink-cursor-mode -1)
(tool-bar-mode -1)

;;; Theme
(let ((host (system-name)))
  (cond ((not (display-graphic-p)) (load-theme 'modus-vivendi t))
        ;; ((member host '("asm-3" "rpi5-mk" "rpzero2" "srvvm"))
        ;;  (load-theme 'tango t))
        ((equal host "mac-asm3.local") (load-theme 'kaolin-breeze t))
        (t (load-theme 'modus-operandi-tinted t))))

;;; Font
(let ((font (cond ((eq system-type 'gnu/linux) "Fira Code-13")
                  ((eq system-type 'android) "Fira Code-14") ;; monospace-14 default font
                  ((eq system-type 'darwin)  "Inconsolata LGC-16")
                  (t "Consolas-12"))))
  (add-to-list 'default-frame-alist (cons 'font font)) ; new frames
  (when (display-graphic-p)
    (set-frame-font font nil t)))                      ; current frame

;;; Popup windows (ECB-like compilation window)
(require 'popwin)
(popwin-mode 1)

(setq popwin:special-display-config
      '(("*Help*" :position right :width 40 :stick t)
        ("*Messages*" :position bottom :height 10 :stick t)
        ("*compilation*" :position bottom :height 15 :stick t :regexp t)
        ("*eshell*" :position bottom :height 15 :stick t)
        ("^\\*helpful.*" :position right :width 0.4 :stick t :regexp t)))

(defvar my/window-max-height 25
  "Height of a bottom popup window when it is active.")

(defvar my/window-min-height 10
  "Height of a bottom popup window when it is not active.")

(defun my/adjust-popwin-windows (&optional _frame)
  "Enlarge the selected bottom popup window and shrink the other ones."
  (when (cdr (window-list))             ; more than one window
    (let ((active (selected-window)))
      (dolist (win (window-list))
        (let* ((buf (window-buffer win))
               (config (and buf (cdr (assoc (buffer-name buf)
                                            popwin:special-display-config)))))
          (when (and config
                     (eq (plist-get config :position) 'bottom)
                     (not (eq win (frame-root-window))))
            (ignore-errors
              (with-selected-window win
                (if (eq win active)
                    (enlarge-window (- my/window-max-height (window-height)))
                  (shrink-window (- (window-height) my/window-min-height)))))))))))

(add-hook 'window-selection-change-functions #'my/adjust-popwin-windows)

;;; screen.el ends here
