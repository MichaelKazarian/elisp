;;; emacs-rc-elisp.el --- LISP-mode settings

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(defun my-lisp-mode-hook ()
  (setq tab-width 2
        indent-tabs-mode nil)
  (abbrev-mode 1)
  (local-set-key [return] 'newline-and-indent)
  ;; (linum-mode 1)
  (display-line-numbers-mode)
  (local-set-key (kbd "C-;") 'comment-or-uncomment-region)
  (local-set-key (kbd "<f7>") 'highlight-symbol-next-in-defun)
  (local-set-key (kbd "S-<f7>") 'highlight-symbol-prev-in-defun))
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

(add-hook 'emacs-lisp-mode-hook
	  '(lambda()
	     (eldoc-mode 1) 
	     (outline-minor-mode 1)
       (indent-guide-mode 1)
       (yas-minor-mode)
	     ))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
;; Perhaps highlight-thing.el will better?
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-nav-mode)


;; (add-hook 'slime-mode-hook  '(lambda()  
;; (define-key slime-mode-map "\r" 'newline-and-indent)))

;;; emacs-rc-elisp.el ends here
