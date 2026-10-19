;;; kbd.el --- Key bindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

;;; Global bindings
(global-set-key (kbd "C-c g") #'goto-line)
(global-set-key (kbd "C-z")   #'undo-only)
(global-set-key (kbd "M-z")   #'undo-redo)   ; Emacs 28+, replaces redo.el
(global-set-key (kbd "<f2>")   #'save-buffer)
(global-set-key (kbd "C-<f2>") #'write-file)
(global-set-key (kbd "<f9>")   #'compile)
(global-set-key (kbd "C-<f10>") #'menu-bar-mode)
(global-set-key (kbd "<f12>")   #'dabbrev-completion)
(global-set-key (kbd "C-<f12>") #'dabbrev-expand)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;;; Hideshow: only where hs-minor-mode is active, so that C--, C-0 and C-9
;;; keep working as numeric prefix arguments everywhere else.
(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-=") #'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C--") #'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-0") #'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-9") #'hs-hide-all))

;;; Cyrillic layout: C-/M-/C-M- chords are translated to their Latin
;;; equivalents (C-ч C-ы -> C-x C-s, M-ц -> M-w, ...), so every command
;;; works without binding it twice.  Plain letters are untouched, so typing
;;; Cyrillic text is not affected.
(let ((cyr "йцукенгшщзфывапролдячсмитьі")
      (lat "qwertyuiopasdfghjklzxcvbnms"))
  (dotimes (i (length cyr))
    (dolist (mods '((control) (meta) (control meta)))
      (define-key key-translation-map
                  (vector (event-convert-list (append mods (list (aref cyr i)))))
                  (vector (event-convert-list (append mods (list (aref lat i)))))))))

;; Plain letters after C-x (C-x b, C-x o)
(define-key ctl-x-map (kbd "и") #'ido-switch-buffer)
(define-key ctl-x-map (kbd "щ") #'other-window)

;;; kbd.el ends here
