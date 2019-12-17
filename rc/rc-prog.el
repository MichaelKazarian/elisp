;;; rc-prog.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(defun my-prog-mode-hook ()
  (setq tab-width 2
        indent-tabs-mode nil)
  (abbrev-mode 1)
  (electric-pair-mode)
  (outline-minor-mode 1)
  (display-line-numbers-mode 1)
  (highlight-symbol-mode 1) ;; Perhaps highlight-thing.el will better?
  (highlight-symbol-nav-mode 1)
  ;; (setq compilation-scroll-output t) ;; Compilation buffer auto scrol
  (setq compilation-scroll-output 'first-error) ;; scroll to the first error
  (local-set-key (kbd "\e\em") 'company-complete)
  (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (local-set-key (kbd "<f7>") 'highlight-symbol-next-in-defun)
  (local-set-key (kbd "S-<f7>") 'highlight-symbol-prev-in-defun))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)


;;; Highlight indentation

;; highlight-indentation-mode colors
(add-hook 'highlight-indentation-mode-hook
          (lambda ()
            ;; (set-face-background 'highlight-indentation-face "gray25")
            ;; (set-face-background 'highlight-indentation-current-column-face "gray25")
            (set-face-background 'highlight-indentation-face "Khaki2")
            (set-face-background 'highlight-indentation-current-column-face "Khaki3")
            ))

;; Highlight currentline settings
(require 'hl-line)
(set-face-background 'hl-line "LightGoldenrod2")
;(set-face-background 'hl-line "SlateBlue4")
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

;; ;;; Add markers to the fringe for regions foldable by hideshow.el
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

;; (autoload 'hideshowvis-minor-mode
;;   "hideshowvis"
;;   "Will indicate regions foldable with hideshow in the fringe."
;;   'interactive)

;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'java-mode-hook
;;                     'python-mode-hook
;;                     'c-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))
;; ;;;

(setq w3m-default-display-inline-images t) ;; w3m show pictures

;; Remind current line status by current buffer.
(global-line-reminder-mode t)

;;; rc-prog.el ends here
