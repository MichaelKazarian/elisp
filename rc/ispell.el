;;; emacs-rc-ispell.el --- Spell check

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(setq ispell-local-dictionary "english"); default dictionary
; enable tex parser, also very helpful
(setq ispell-enable-tex-parser t)
(add-hook 'text-mode-hook 'flyspell-mode)
(setq flyspell-default-dictionary "english")
(setq flyspell-delay '2)

(custom-set-variables
 '(ispell-choices-win-default-height 5)
 '(ispell-local-dictionary "english" t)
 '(ispell-dictionary-alist '(
                             ("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1)
                             ("ukrainian" "[[:alpha:]]" "[^[:alpha:]]" "['`-]" t  ("-d" "uk_UA"))))
 )
;;; emacs-rc-ispell.el ends here

