;;; emacs-rc-cedet.el 

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

; (load-file "~/elisp/mode/cedet/cedet-devel-load.el") ;d
;;(add-to-list 'load-path "~/projects/cedet-bzr/contrib/")

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; ;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) ;d
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)

;(semantic-load-enable-code-helpers)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(semantic-mode 1)

;;(require 'cedet-java)
(require 'semantic/ia)
;; (require 'semantic/java)
(require 'semantic/wisent)
(require 'eieio)
(global-ede-mode t)
(setq-default semantic-symref-tool 'grep)

;;; emacs-rc-cedet.el ends here
