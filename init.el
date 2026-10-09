;; -*- lexical-binding: t; -*-
;; Init file for custom settings
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(add-to-list 'load-path "~/elisp/mode")
(add-to-list 'load-path "~/elisp/rc")

(defconst my/profiles
  '((base   :parent nil
            :packages (popwin marginalia
                              mkdown markdown-preview-mode markdown-preview-eww
                              highlight-symbol auto-highlight-symbol highlight-thing
                              line-reminder imenu-list yasnippet yasnippet-snippets
                              doom-modeline sunny-day-theme
                              git-gutter fringe-helper
                              dash s ht ov)
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

(defun my/profile-chain (p)
  "Список профілів від кореня до P."
  (let ((parent (plist-get (alist-get p my/profiles) :parent)))
    (append (when parent (my/profile-chain parent)) (list p))))

(defun my/profile-collect (key)
  (apply #'append
         (mapcar (lambda (p) (plist-get (alist-get p my/profiles) key))
                 (my/profile-chain my/profile))))

;; Пакети: активуємо тільки потрібні профілю
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-load-list
      (mapcar (lambda (p) (list p t)) (my/profile-collect :packages)))
(package-initialize)

(setq custom-file (expand-file-name
                   (format "custom-%s.el" my/profile) user-emacs-directory))
(setq warning-minimum-level :error
      tramp-default-method "ssh")

;; rc-файли профілю (спочатку базові, потім дочірні)
(dolist (f (my/profile-collect :rc))
  (condition-case err
      (load (expand-file-name f "~/elisp/rc") nil nil)
    (error (message "RC %s FAILED: %S" f err))))

(when (file-exists-p custom-file) (load custom-file nil t))
