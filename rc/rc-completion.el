;;; rc-completion.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

;; Default emacs completions
;; https://www.reddit.com/r/emacs/comments/xz6oq8/me_default_emacs_completions_are_good_i_swear/
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)

(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)

(defun my/minibuffer-choose-completion (&optional no-exit no-quit)
  (interactive "P")
  (with-minibuffer-completions-window
    (let ((completion-use-base-affixes nil))
      (choose-completion nil no-exit no-quit))))

(define-key completion-in-region-mode-map (kbd "M-RET") 'my/minibuffer-choose-completion)

;; marginalia-mode
(marginalia-mode t)
(setq marginalia-field-width 50)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "\e\em") 'company-complete)
(company-quickhelp-mode)
(setq company-quickhelp-delay 3)
(setq company-idle-delay nil)

;;; rc-completion.el ends here
