;;; rc-completion.el --- Built-in completion  -*- lexical-binding: t; -*-

;; Copyright (C) Michael Kazarian
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Status: not intended to be distributed yet

;; Default Emacs completion (*Completions* buffer), Emacs 29+
;; https://www.reddit.com/r/emacs/comments/xz6oq8/me_default_emacs_completions_are_good_i_swear/

;;; *Completions* window
(setq completions-format 'one-column
      completions-header-format nil
      completions-max-height 20
      completion-auto-select 'second-tab) ; 1st TAB shows the list, 2nd enters it

;;; Matching
(setq completion-styles '(basic partial-completion flex)
      completion-category-overrides '((file (styles basic partial-completion)))
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;; In buffers: TAB indents, then completes
(setq tab-always-indent 'complete)

;;; Emacs 30+ (guarded, so the same config still loads on older Emacs)
(when (>= emacs-major-version 30)
  (setq completions-sort 'historical          ; recently used candidates first
        minibuffer-visible-completions t      ; up/down/RET work on the visible list
        completion-preview-minimum-symbol-length 2)
  (add-hook 'prog-mode-hook #'completion-preview-mode)) ; ghost text, TAB accepts

;;; Keys
(define-key minibuffer-mode-map (kbd "C-n") #'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)

(defun my/minibuffer-choose-completion (&optional no-exit no-quit)
  "Choose the current candidate of the *Completions* window."
  (interactive "P")
  (with-minibuffer-completions-window
    (let ((completion-use-base-affixes nil))
      (choose-completion nil no-exit no-quit))))

(define-key completion-in-region-mode-map (kbd "M-RET") #'my/minibuffer-choose-completion)

;;; Annotations
(marginalia-mode 1)
(setq marginalia-field-width 50)

;; `marginalia-annotate-symbol' is the only annotator that appends the file
;; the symbol comes from (…/emacs-lisp/byte-run.elc); cut that last field.
(defun my/marginalia-annotate-symbol (cand)
  "Like `marginalia-annotate-symbol', but without the trailing file name."
  (when-let* ((str (marginalia-annotate-symbol cand)))
    (if-let* ((pos (text-property-any 0 (length str)
                                      'face 'marginalia-file-name str)))
        (string-trim-right (substring str 0 pos))
      str)))

(with-eval-after-load 'marginalia
  (setf (alist-get 'symbol marginalia-annotators)
        '(my/marginalia-annotate-symbol builtin none)))

;;; rc-completion.el ends here
