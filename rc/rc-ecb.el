;;; emacs-rc-ecb.el --- Emacs Code Browser settings

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com

;(require 'ecb)
(require 'ecb-autoloads)

(defun ecb-display-buffer-at-bottom-around (orig-fun &rest args)
  "Work around ECB frame handling of `display-buffer-at-bottom'."
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (apply #'display-buffer-use-some-window args)
    (apply orig-fun args)))

(advice-add 'display-buffer-at-bottom
            :around #'ecb-display-buffer-at-bottom-around)

;; Diagnostics.
(setq stack-trace-on-error t)

;; Key bindings.
(global-set-key (kbd "\e\el")  #'ecb-toggle-ecb-windows)
(global-set-key (kbd "\e\eea") #'ecb-activate)
(global-set-key (kbd "\e\eed") #'ecb-deactivate)

;; General settings.
(setq ecb-auto-activate nil
      ecb-auto-compatibility-check nil
      ecb-cache-directory-contents '((".*" . 50))
      ecb-directories-menu-user-extension-function nil
      ecb-gzip-setup 'cons
      ecb-history-menu-user-extension-function nil
      ecb-layout-name "left14"
      ecb-methods-menu-user-extension-function nil
      ecb-prescan-directories-for-emptyness nil
      ecb-sources-menu-user-extension-function nil
      ecb-tar-setup 'cons
      ecb-tip-of-the-day nil
      ecb-version-check nil
      ecb-windows-height 0.2
      ecb-windows-width 0.2)

;; Compilation window.
(setq ecb-compile-window-height 12
      ecb-compile-window-temporally-enlarge 'after-selection
      ecb-compile-window-width 'edit-window)

;; Buffers displayed in ECB's compilation window.
(setq ecb-compilation-buffer-names
      '(("*shell*" . nil)
        ("\\*[cC]ompilation.*\\*" . t)
        ("\\*i?grep.*\\*" . t)
        ("*Completions*" . nil)
        ("*Backtrace*" . nil)
        ("*Compile-log*" . nil)
        ("*Messages*" . nil)))

;;; emacs-rc-ecb.el ends here
