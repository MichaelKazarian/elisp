;;; emacs-rc-screen.el ---

;;; General screen settings

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(setq inhibit-startup-message t);;Загружаемся молча
(setq column-number-mode t)
;;Line numbering
(setq line-number-mode t)

;; (when (version<= "26.0.50" emacs-version )
;;   (global-display-line-numbers-mode))
;(global-linum-mode 1)

;;Scroll
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)
(setq scroll-margin 10)
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;; hour format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq calendar-date-display-form (quote ((format "%04s-%02d-%02d" year (string-to-int month) (string-to-int day)))))
(setq calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))))
(setq calendar-week-start-day 1)
(setq european-calendar-style t)
;(display-time-mode t)

;; Paste at point NOT at cursor
(setq mouse-yank-at-point 't)
(mouse-wheel-mode 1) ;mouse scroll
(blink-cursor-mode nil) ; Blinking cursors 

;; Shut off message buffer. Note - if you need to debug emacs, comment these out so you can see what's going on.
(setq message-log-max t);;0 - FOR SLIME elsi nil

;;(menu-bar-mode -1)
(tool-bar-mode -1);;Emacs 23
;(set-default 'truncate-lines t) ;;Long lines not trunkate

;; Color theme
(setq host (system-name))
(cond ((not (display-graphic-p)) (load-theme 'tango-dark t))
      ((member host '("asm-3" "rpi5-mk" "rpzero2" "srvvm")) (load-theme 'kaolin-mono-light t))
      ((equal host "mac-asm3.local") (load-theme 'kaolin-breeze t))
      (t (load-theme 'sunny-day t)))

;;Font
(require 'font-lock)
(cond ((equal system-type 'gnu/linux) (setq ttf-font "FiraCode"))
      ((equal system-type 'darwin) (setq ttf-font "Inconsolata LGC"))
      (t (setq ttf-font "Consolas")))
(cond ((equal system-type 'gnu/linux) (setq ttf-font-s 115))
      ((equal system-type 'darwin) (setq ttf-font-s 180))
      (t (setq ttf-font-s 120)))
(set-face-attribute 'default nil
                    :family ttf-font :height ttf-font-s :weight 'normal)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq unibyte-display-via-language-environment t)

;; Add behaviour like ECB compilation window
(require 'popwin)
(popwin-mode 1)

(setq popwin:special-display-config
      '(("*Help*" :position right :width 40 :stick t)
        ("*Messages*" :position bottom :height 10 :stick t)
        ("*compilation*" :position bottom :height 15 :stick t :regexp t)
        ("*eshell*" :position bottom :height 15 :stick t)
        ("^\\*helpful.*" :position right :width 0.4 :stick t :regexp t)
        ))

(defvar my-window-max-height 25
  "Height of the window when it is active.")

(defvar my-window-min-height 10
  "Minimum height of the window when it is not active.")

(defun my-adjust-popwin-windows ()
  "Minimum height of the window when it is not active."
  (dolist (win (window-list))
    (let ((buf (window-buffer win)))
      (when (and buf
                 (assoc (buffer-name buf) popwin:special-display-config))
        (let ((config (cdr (assoc (buffer-name buf) popwin:special-display-config))))
          (when (eq (plist-get config :position) 'bottom)
            (if (eq (selected-window) win)
                (with-selected-window win
                  (enlarge-window (- my-window-max-height (window-height))))
              (with-selected-window win
                (shrink-window (- (window-height) my-window-min-height))))))))))

(add-hook 'window-selection-change-functions
          (lambda (_) (my-adjust-popwin-windows)))

;;; emacs-rc-screen.el ends here
