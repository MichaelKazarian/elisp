;;; emacs-rc-screen.el --- Editing screen

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
;(load-theme 'material t)
(add-to-list 'custom-theme-load-path "~/elisp/rc/my-themes")
;(load-theme 'alect-light t)
(load-theme 'monokai t)
;(load-theme 'mccarthy t)
;(load-theme 'Deviant t) ;;https://github.com/Corsair/emacs-deviant-theme/blob/master/Deviant-theme.el

;; highlight-indentation-mode colors
(add-hook 'highlight-indentation-mode-hook
          (lambda ()
            (set-face-background 'highlight-indentation-face "gray25")
            (set-face-background 'highlight-indentation-current-column-face "gray25")
            ;; (set-face-background 'highlight-indentation-face "#e3e3d3")
            ;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
            ))
;;Font
(require 'font-lock)
(setq ttf-font "-*-Inconsolata LGC-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
;(setq ttf-font "-*-PT Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
;(setq ttf-font "-*-Consolas-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
;(setq ttf-font "-*-DejaVu Sans Mono-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
(cond ((equal system-type 'gnu/linux)
       (if (>= emacs-major-version 23)
           (set-default-font ttf-font)
         ;;(set-default-font "-misc-*-*-*-*-*-18-*-*-*-*-*-*")
         ))
      ((equal system-type 'windows-nt) (set-default-font ttf-font)))

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq unibyte-display-via-language-environment t)

;; Highlight currentline settings
(require 'hl-line)
(set-face-background 'hl-line "SlateBlue4")
(set-face-attribute hl-line-face nil :underline nil)
;(set-face-background 'hl-line "only line")
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook 'hl-line-mode)

;; Whitespace settings
(whitespace-mode 1)
(setq whitespace-global-mode nil)
(setq whitespace-silent t)
(setq whitespace-modes (quote (awk-mode)))
(setq whitespace-style '(spaces tabs newline space-mark tab-mark face))
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray70")

(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

;;; Add markers to the fringe for regions foldable by hideshow.el
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'java-mode-hook
                    'python-mode-hook
                    'c-mode-hook))
  (add-hook hook 'hideshowvis-enable))
;;;

(setq w3m-default-display-inline-images t) ;; w3m show pictures

;; Remind current line status by current buffer.
(global-line-reminder-mode t)

;;; emacs-rc-screen.el ends here
