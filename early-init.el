;; -*- lexical-binding: t; -*-

;; Disable native compilation of Emacs Lisp.
(setq native-comp-speed 0)
(setq native-comp-jit-compilation nil)
(setq native-comp-deferred-compilation nil)

(defvar my/profile (intern (or (getenv "EMACS_PROFILE") "reader")))

;; Автоматичний package-initialize вимикаємо: викличемо його самі,
;; коли вже знатимемо список пакетів профілю.
(setq package-enable-at-startup nil)

;; Швидший старт: тимчасово піднімаємо поріг GC
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(defconst my/termux-p
  (and (or (getenv "TERMUX_VERSION")
           (string-match-p "com\\.termux" (or (getenv "PREFIX") "")))
       (file-directory-p "/data/data/com.termux/files/home"))
  "Non-nil when Emacs runs inside Termux.")

(when my/termux-p
  ;; 1. Point Emacs to the config directory in Termux
  (setenv "HOME" "/data/data/com.termux/files/home")
  (setq user-emacs-directory "/data/data/com.termux/files/home/elisp/.emacs.d/")

  ;; 2. Add Termux binaries to PATH so git, python, pdftotext etc. work
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin" (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))
