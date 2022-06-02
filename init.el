;; Init file for custom settings
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


;; Some useful packages:
;; color-theme-sanityinc-solarized doom-themes nova-theme
;; imenu-list map e2wm olivetti popper
;; elpy

;; keep init.el clean. Write to custom.el
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; My personal info
;(setq frame-title-format "emacs@Michael.Kazarian")
;(setq user-full-name "Michael Kazarian")
;(setq my-author-name (getenv "USER"))
;(setq user-full-name (getenv "USER"))

(setq tramp-default-method "ssh")

(add-to-list 'load-path "~/elisp/mode")
(add-to-list 'load-path "~/elisp/rc")
(load "~/elisp/rc/editing.el")
(load "~/elisp/rc/screen.el")
(load "~/elisp/rc/vcs.el")
(load "~/elisp/rc/rc-prog.el")
;; programming tools & languages
(load "~/elisp/rc/lisp.el")
(load "~/elisp/rc/tags.el")
(load "~/elisp/rc/rc-cedet.el")
(load "~/elisp/rc/rc-ecb.el")
(load "~/elisp/rc/rc-ispell.el")
(load "~/elisp/rc/rc-eshell.el")
(load "~/elisp/rc/rc-nxml.el")
(load "~/elisp/rc/rc-json.el")
(load "~/elisp/rc/rc-python.el")
(load "~/elisp/rc/java.el")
(load "~/elisp/rc/markdown.el")
(load "~/elisp/rc/rc-org.el")
(load "~/elisp/rc/rc-make.el")

;; others tools
(load "~/elisp/rc/auto-insert.el")
(load "~/elisp/rc/backup.el")
(load "~/elisp/rc/savehistory")
(load "~/elisp/rc/kbd.el")
(load "~/elisp/rc/rc-company.el")

