;;; vcs.el --- Version control: Magit, Mercurial (ahg), git-gutter  -*- lexical-binding: t; -*-

;; Copyright (C) Michael Kazarian
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Status: not intended to be distributed yet

;;; Git
(global-set-key (kbd "C-x g") #'magit-status)

;; git-gutter hunk commands (moved here from rc-prog.el)
(define-key prog-mode-map (kbd "\e\eg=") #'git-gutter:popup-hunk)
(define-key prog-mode-map (kbd "\e\egs") #'git-gutter:stage-hunk)
(define-key prog-mode-map (kbd "\e\egr") #'git-gutter:revert-hunk)

;;; Mercurial: load ahg only where hg is installed
(when (executable-find "hg")
  (require 'ahg))

;;; vcs.el ends here
