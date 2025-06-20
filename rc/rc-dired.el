;;; rc-dired.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
(add-to-list 'load-path (expand-file-name "~/elisp/mode/dired-sort"))
(require 'dired-lister)
(require 'dired-sort)
(dired-lister-mode 1)
(dired-sort-mode 1)

;;; rc-dired.el ends here
