;;; emacs-rc-kbd.el --- Editing key bindings

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-z" 'undo)
(global-set-key (kbd"M-z") 'redo)
(global-set-key [f2] 'save-buffer)
(global-set-key [C-f2] 'write-file)
(global-set-key [f12] 'dabbrev-completion)
(global-set-key [C-f12] 'dabbrev-expand)
(global-set-key [f9] 'compile)
(global-set-key [C-f10] 'menu-bar-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd"C-=") 'hs-show-block)
(global-set-key (kbd"C--") 'hs-hide-block)
(global-set-key (kbd"C-0") 'hs-show-all)
(global-set-key (kbd"C-9") 'hs-hide-all)
;; Cyrillic keys
(global-set-key [?\C-ч ?\C-ы] 'save-buffer)     ; C-x C-s
(global-set-key [?\C-ч ?\C-і] 'save-buffer)     ; C-x C-s
(global-set-key [?\C-ч ?\C-и] 'ibuffer)         ; C-x C-b
(global-set-key [?\C-ч ?\и] 'iswitchb-buffer)   ; C-x b
(global-set-key [?\C-ч ?\щ] 'other-window)      ; C-x o
(global-set-key [?\C-я] 'undo) ;C-z
(global-set-key [?\M-я] 'redo) ;M-z
(global-set-key [?\C-п] 'keyboard-quit)   ; C-g
(global-set-key [?\C-і] 'isearch-forward) ;C-s
(global-set-key [?\C-ы] 'isearch-forward) ;C-s
(global-set-key [?\C-к] 'isearch-backward) ;C-r
(global-set-key [?\M-ц] 'kill-ring-save) ; M-w
(global-set-key [?\C-ц] 'kill-region)      ; C-w
(global-set-key [?\C-н] 'yank)      ; C-y
(global-set-key [?\C-е] 'move-end-of-line)      ; C-e
(global-set-key [?\C-а] 'move-beginning-of-line)        ; C-a
(global-set-key [?\C-л] 'kill-line)     ; C-k

;;; emacs-rc-kbd.el ends here
