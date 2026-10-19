;;; rc-prog.el --- Programming settings  -*- lexical-binding: t; -*-

;; Copyright (C) Michael Kazarian
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Status: not intended to be distributed yet

;;; Tags
(defvar tags-candidate '(".")
  "Directories searched for source files when building tags.")

(defun find-dirs-str ()
  "Returns tags-candidate prepared to find command"
  (let (value)
    (dolist (element tags-candidate value)
      (setq value (concat element " " value)))))

;;; prog-mode
(setq compilation-scroll-output 'first-error) ; scroll to the first error
(electric-pair-mode 1)

(dolist (fn '(abbrev-mode
              outline-minor-mode
              display-line-numbers-mode
              highlight-symbol-mode
              highlight-symbol-nav-mode))
  (add-hook 'prog-mode-hook fn))

(define-key prog-mode-map (kbd "C-;")    #'comment-or-uncomment-region)
(define-key prog-mode-map (kbd "<f7>")   #'highlight-symbol-next-in-defun)
(define-key prog-mode-map (kbd "S-<f7>") #'highlight-symbol-prev-in-defun)

;;; Highlight current line
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :underline nil))
(dolist (hook '(dired-mode-hook package-menu-mode-hook
                Buffer-menu-mode-hook ibuffer-mode-hook))
  (add-hook hook #'hl-line-mode))

;;; Whitespace (toggle by hand with M-x whitespace-mode)
(with-eval-after-load 'whitespace
  (setq whitespace-silent t
        whitespace-style '(spaces tabs newline space-mark tab-mark face)
        ;; numbers are Unicode codepoints in decimal
        whitespace-display-mappings
        '((space-mark 32 [183] [46])    ; SPACE -> MIDDLE DOT, FULL STOP
          (newline-mark 10 [182 10])    ; LINE FEED -> PILCROW
          (tab-mark 9 [9655 9] [92 9]))) ; TAB -> WHITE RIGHT-POINTING TRIANGLE
  (set-face-attribute 'whitespace-space nil
                      :background 'unspecified :foreground "gray70"))

;;; Fold markers in the fringe (graphical frames only)
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook '(emacs-lisp-mode-hook java-mode-hook
                python-mode-hook c-mode-hook))
  (add-hook hook #'hideshowvis-enable))

;;; imenu-list  https://github.com/bmag/imenu-list
(setq imenu-list-position 'left)
(global-set-key (kbd "\e\el") #'imenu-list-smart-toggle)

;;; rc-prog.el ends here
