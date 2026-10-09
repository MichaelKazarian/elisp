;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/elisp/mode")
(add-to-list 'load-path "~/elisp/rc")

(defconst my/profiles
  '((base   :parent nil
            :packages (compat mkdown markdown-preview-mode markdown-preview-eww
                              highlight-symbol auto-highlight-symbol highlight-thing
                              line-reminder imenu-list yasnippet yasnippet-snippets
                              doom-modeline sunny-day-theme
                              git-gutter fringe-helper
                              dash s ht ov compat popwin marginalia)
            :rc ("editing" "screen" "rc-mouse" "rc-completion"
                 "markdown" "rc-org" "backup" "savehistory" "kbd"))
    (reader :parent base
            :packages (nov esxml)
            :rc ("rc-nov" "rc-pdf-text"))
    (dev    :parent base
            :packages (magit magit-section with-editor llama cond-let
                             elpy pyvenv highlight-indentation
                             highlight-function-calls
                             posframe pos-tip
                             company company-quickhelp company-quickhelp-terminal
                             eproject aproject projector isearch-project vmd-mode ahg)
            :rc ("vcs" "rc-prog" "lisp" "tags" "rc-clang" "rc-eshell"
                 "rc-nxml" "rc-json" "rc-python" "java"
                 "rc-make" "rc-javascript" "auto-insert" "rc-dired"))))

(defun my/profile-chain (profile)
  "Return the list of profiles from the root ancestor down to PROFILE.
For example, with `dev' inheriting from `base', this returns (base dev)."
  (let ((parent (plist-get (alist-get profile my/profiles) :parent)))
    (append (when parent (my/profile-chain parent)) (list profile))))

(defun my/profile-collect (key)
  "Collect KEY values (e.g. :packages or :rc) across the current profile chain.
Values from parent profiles come first, followed by those of the child."
  (apply #'append
         (mapcar (lambda (p) (plist-get (alist-get p my/profiles) key))
                 (my/profile-chain my/profile))))

;; Packages: activate only those needed by the profile
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-load-list
      (mapcar (lambda (p) (list p t)) (my/profile-collect :packages)))
(package-initialize)

(setq custom-file (expand-file-name
                   (format "custom-%s.el" my/profile) user-emacs-directory))
(setq warning-minimum-level :error
      tramp-default-method "ssh")

;; Profile rc files (base ones first, then child ones)
(dolist (f (my/profile-collect :rc))
  (condition-case err
      (load (expand-file-name f "~/elisp/rc") nil nil)
    (error (message "RC %s FAILED: %S" f err))))

(when (file-exists-p custom-file) (load custom-file nil t))
