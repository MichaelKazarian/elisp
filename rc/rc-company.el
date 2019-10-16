;;; emacs-rc-company.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "\e\em") 'company-complete)
(setq company-quickhelp-delay 4)
(company-quickhelp-mode)

;;; emacs-rc-company.el ends here
