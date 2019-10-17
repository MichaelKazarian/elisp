;;; emacs-rc-python.el --- Python-mode

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

;;; ELPY
(elpy-enable)
;(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;; ELPY END

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (setq tab-width 4)
            ;; (linum-mode 1)
            (display-line-numbers-mode)
            (highlight-indentation-mode)
            (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
            (electric-pair-mode)
            (outline-minor-mode 1)
            (local-set-key (kbd "\e\em") 'company-complete)
            (local-set-key [f8] 'flycheck-mode)
            ))

;;; emacs-rc-python.el ends here
