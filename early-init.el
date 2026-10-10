;; -*- lexical-binding: t; -*-
(defvar my/profile (intern (or (getenv "EMACS_PROFILE") "reader")))

;; Автоматичний package-initialize вимикаємо: викличемо його самі,
;; коли вже знатимемо список пакетів профілю.
(setq package-enable-at-startup nil)

;; Швидший старт: тимчасово піднімаємо поріг GC
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; 1. Перенаправляємо Emacs на вашу папку в Termux
(setenv "HOME" "/data/data/com.termux/files/home")
(setq user-emacs-directory "/data/data/com.termux/files/home/elisp/.emacs.d/")

;; 2. Обов'язково додаємо бінарники Termux у PATH, щоб працювали git, python тощо
(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin" (getenv "PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)
