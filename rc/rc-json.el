;;; rc-json.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)
            (highlight-indentation-mode)
            (whitespace-mode 1)
            (local-set-key [f8] 'flycheck-mode)
            ))

;;; rc-json.el ends here
