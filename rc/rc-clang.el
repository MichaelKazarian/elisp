;;; rc-clang.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(setq clang-tags-candidate '("."))

(defun get-h-dirs (dest)
  "Returns directories list contains *.h files starts from dest"
  (let ((cmd (concat "find "
                     dest
                     " -type f -name '*.h' -printf '%h\n' | uniq")))
    (delete '"" (split-string (shell-command-to-string cmd) "\n"))))

(defun clang-src-dirs ()
  "Returns tags-candidate as clang -I arguments"
  (let (value)
    (dolist (element clang-tags-candidate value)
      (dolist (dir (get-h-dirs element))
        (setq value (cons (concat "-I" (expand-file-name dir)) value)))
      )))

;;; rc-clang.el ends here
