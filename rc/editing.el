;;; emacs-rc-editing.el --- Editing settings

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

(custom-set-variables
 '(case-fold-search t)
 '(default-input-method "cyrillic-jcuken")
 '(delete-selection-mode t)
 '(fill-column 90)
 '(kill-whole-line t)
 '(tab-always-indent t)
 '(indent-tabs-mode nil)
 '(transient-mark-mode t)
 '(tab-width 2)
 '(safe-local-variable-values
   (quote ((c-file-offsets (
                            substatement-open . 0))
           (prompt-to-byte-compile)
           (c-indentation-style . k&r)
           (folded-file . t))))
 '(show-paren-mode t)
; '(next-line-add-newlines t)
; '(pc-select-meta-moves-sexps t)
; '(pc-selection-mode t nil (pc-select))
 '(pc-select-selection-keys-only t) ;disable auto
 '(auto-save-default nil)
 )
;;set paren colour
;(set-face-background 'show-paren-match (face-background 'default))
(set-face-background 'show-paren-match "goldenrod")
;(set-face-foreground 'show-paren-match "olive grab")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(put 'upcase-region 'disabled nil)

(require 'redo)
;(require 'popup-ruler)
;(require 'bookmark+);;e42 dis
;(require 'highlight-current-line);;e42 dis
(require 'ibuffer) ;;Buffer advansed management

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;;Switch buffers
(require 'iswitchb)
(iswitchb-mode 1)
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*Backtrace")
(add-to-list 'iswitchb-buffer-ignore "*Quail Com")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*fsm-debug")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")
(add-to-list 'iswitchb-buffer-ignore "*slime-events*")

;; Display the overlay content in a tooltip
(defun display-code-line-counts (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (overlay-put ov 'help-echo
                     (buffer-substring (overlay-start ov)
 		                      (overlay-end ov)))))
(setq hs-set-up-overlay 'display-code-line-counts)
;;; emacs-rc-editing.el ends here

