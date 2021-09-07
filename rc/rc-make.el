;;; rc-make.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet


(defun my-make-mode-hook ()
  (setq indent-tabs-mode t)
  (whitespace-mode 1)
  )

(add-hook 'makefile-mode-hook 'my-make-mode-hook)

;;; rc-make.el ends here
