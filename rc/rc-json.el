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
            ;; (highlight-indentation-mode)
            (indent-guide-mode 1)
            (whitespace-mode 1)
            (local-set-key (kbd "\e\ep") 'json-point)
            (local-set-key (kbd "\e\eз") 'json-point)
            (local-set-key [f8] 'flycheck-mode)
            (local-set-key (kbd "C-<f7>") 'lesson-grep)
            (local-set-key (kbd "\e\ep") 'json-point)
            ))

;;; rc-json.el ends here
