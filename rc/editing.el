;;; editing.el --- Editing settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

;;; Basics
(setq-default fill-column 90
              tab-width 2
              indent-tabs-mode nil
              default-input-method "cyrillic-jcuken")
(setq kill-whole-line t
      auto-save-default nil)

(delete-selection-mode 1)
(show-paren-mode 1)
(save-place-mode 1)                     ; remember cursor position
(put 'upcase-region 'disabled nil)

(setq safe-local-variable-values
      '((c-file-offsets (substatement-open . 0))
        (prompt-to-byte-compile)
        (c-indentation-style . k&r)
        (folded-file . t)))

;;; Parens
(set-face-attribute 'show-paren-match nil
                    :background "goldenrod" :weight 'extra-bold)

;;; Switch buffers
(ido-mode 'buffers)                     ; buffers only, files stay default
(setq ido-ignore-buffers
      '("\\` " "\\*Messages\\*" "\\*Backtrace" "\\*Quail Com" "\\*Buffer"
        "\\*fsm-debug" "\\*Completions" "\\`[tT][aA][gG][sS]\\'"
        "\\*slime-events\\*"))

;;; Hideshow: show the folded text in a tooltip
(defun my/hs-overlay-tooltip (ov)
  "Show the hidden text of overlay OV as its tooltip."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov) (overlay-end ov)))))
(setq hs-set-up-overlay #'my/hs-overlay-tooltip)

;;; editing.el ends here
