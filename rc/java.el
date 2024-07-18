;;; emacs-rc-java.el 

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet
;;
(defun my-java-mode-hook ()
  ;; (local-set-key (kbd "RET") 'newline-and-indent)
  (display-line-numbers-mode)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (local-set-key [f8] 'flycheck-mode)
  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))
  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '+)
  (yas-minor-mode))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;;; emacs-rc-java.el ends here
