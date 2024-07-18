;;; rc-javascript.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet
;; See https://ternjs.net/ and https://wavesurfer.xyz/blog/emacs-javascript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-hook 'js-mode-hook (lambda ()
                         (tern-mode t)
                         (hideshowvis-enable)))

(custom-set-variables '(js-indent-level 2))

(eval-after-load 'tern
  '(progn
     (autoload 'company-tern "company-tern.el" nil t)
     (add-to-list 'company-backends 'company-tern)
     ;; (require 'tern-auto-complete)
     ;; (tern-ac-setup)
     ;; (auto-complete-mode t)
     ;; (local-set-key (kbd "\e\em") 'tern-ac-complete)
     ))

;;; rc-javascript.el ends here
