;;; rc-cedet.el --- CEDET / Semantic (backend for ECB)  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

;; Semantic has to be loaded before its variables can be customized.
(require 'cedet)
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/wisent)
;; (require 'semantic/java)

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; Optional Semantic features
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
;; (semantic-load-enable-code-helpers)
(add-to-list 'semantic-default-submodes
             'global-semantic-idle-local-symbol-highlight-mode)

(setq-default semantic-symref-tool 'grep)

(semantic-mode 1)

;; Enable EDE only for projects that need it.
;; Add the following to the project's .dir-locals.el:
;;
;; ((nil . ((eval . (ede-minor-mode 1)))))
;;
;; (global-ede-mode 1)

;;; emacs-rc-cedet.el ends here
