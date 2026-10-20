;;; lisp.el --- Lisp modes settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

;; Abbrev, outline, eldoc, RET indentation and tab settings already come
;; from prog-mode, rc-prog.el and Emacs defaults.

;; Common Lisp
(add-hook 'lisp-mode-hook #'indent-guide-mode)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'indent-guide-mode)
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point))

;;; lisp.el ends here
