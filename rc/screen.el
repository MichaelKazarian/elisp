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
(cond ((equal host "asm-3") (load-theme 'kaolin-galaxy t))
      ((equal host "raspberrypi") (load-theme 'kaolin-galaxy t))
      ((equal host "Mac-Admin.local") (load-theme 'kaolin-galaxy t))
      ((equal host "MacBook-Pro-Admin.local") (load-theme 'kaolin-galaxy t))
      (t (load-theme 'sunny-day t)))
;; (add-to-list 'custom-theme-load-path "~/elisp/rc/my-themes")
;; (load-theme 'monokai t)
;; (load-theme 'mccarthy t)
;; (load-theme 'Deviant t) ;;https://github.com/Corsair/emacs-deviant-theme/blob/master/Deviant-theme.el

;;Font
(require 'font-lock)

(cond ((equal system-type 'gnu/linux) (setq ttf-font "Inconsolata LGC"))
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


;;; emacs-rc-screen.el ends here
