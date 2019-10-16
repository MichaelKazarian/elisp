;;; emacs-rc-ecb.el --- Emacs Code Browser settings

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(add-to-list 'load-path
                   "~/elisp/mode/ecb") 
(require 'ecb)
(setq stack-trace-on-error t)
;(require 'ecb-autoloads)
(global-set-key (kbd "\e\el") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "\e\eea") 'ecb-activate)
(global-set-key (kbd "\e\eed") 'ecb-deactivate)

(custom-set-variables
'(ecb-auto-activate nil)
 '(ecb-auto-compatibility-check nil)
 '(ecb-cache-directory-contents (quote ((".*" . 50))))
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*")
                                        ("*Apropos*") ("*Occur*") ("*shell*")
                                        ("\\*[cC]ompilation.*\\*" . t)
                                        ("\\*i?grep.*\\*". t) ("*JDEE Compile Server*")
                                        ("*Help*") ("*Completions*") ("*Backtrace*")
                                        ("*Compile-log*") ("*bsh*") ("*slime-events*")
                                         ("*slime-repl sbcl*") ("*slime-description*")
                                        ("*Messages*") ("*java doc*")
                                        ("*PYDOCS*") ("*Python*") ("*Python Output*"))))
 '(ecb-compile-window-height 12)
 '(ecb-compile-window-temporally-enlarge (quote after-selection))
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-directories-menu-user-extension-function nil)
 '(ecb-gzip-setup (quote cons))
 '(ecb-history-menu-user-extension-function nil)
 '(ecb-layout-name "left15")
 '(ecb-methods-menu-user-extension-function nil)
 '(ecb-options-version "2.4")
 '(ecb-prescan-directories-for-emptyness nil)
 '(ecb-sources-menu-user-extension-function nil)
 '(ecb-tar-setup (quote cons))
 '(ecb-tip-of-the-day nil)
 '(ecb-version-check nil)
 '(ecb-windows-height 0.2)
 '(ecb-windows-width 0.2))
;;; emacs-rc-ecb.el ends here
