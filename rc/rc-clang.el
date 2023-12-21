;;; rc-clang.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(defun clang-dirs ()
  "Returns tags-candidate as clang -I arguments"
  (let (value)
    (dolist (element tags-candidate value)
      (setq value (cons (concat "-I" element) value)))))

;;; rc-clang.el ends here
