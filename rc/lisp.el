;;; lisp.el --- Lisp modes settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(add-to-list 'auto-mode-alist
             '("\\.dir-locals\\(-2\\)?\\.el\\'" . emacs-lisp-mode))

;; Indentation guides for all Lisp modes.
(add-hook 'lisp-data-mode-hook #'indent-guide-mode)

;; Helpful for Emacs Lisp, including .dir-locals.el and *scratch*.
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point))

;;; lisp.el ends here
