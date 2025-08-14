;;; rc-dired.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
(add-to-list 'load-path (expand-file-name "~/elisp/mode/dired-sort"))
(add-to-list 'load-path (expand-file-name "~/elisp/mode/dired-lister"))
(require 'dired-lister)
(dired-lister-mode 1)
(require 'dired-sort)
(dired-sort-mode 1)

;;; rc-dired.el ends here
