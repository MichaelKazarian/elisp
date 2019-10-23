;;; rc-regex.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(require 'foreign-regexp)
(custom-set-variables
'(foreign-regexp/regexp-type 'javascript) ;; Choose your taste of foreign regexp
                                          ;; from 'perl, 'ruby or 'javascript.
'(reb-re-syntax 'foreign-regexp))         ;; Tell re-builder to use foreign regexp.


;;; rc-regex.el ends here
