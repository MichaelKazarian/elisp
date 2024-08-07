;;; emacs-rc-python.el --- Python-mode

;; Copyright (C) 2009 Michael Kazarian
;; Author: michael.kazarian@gmail.com
;; Status: not intended to be distributed yet

;;; ELPY
(setenv "WORKON_HOME" (concat (getenv "HOME") "/.emacs.d/elpy/"))
(setq elpy-rpc-virtualenv-path "~/.emacs.d/elpy/rpc-venv/"
      elpy-rpc-python-command "python3"
      python-shell-interpreter "python3"
      python-shell-interpreter-args "-i"
      python-indent-indent-offset 4)

(elpy-enable)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook
            (lambda ()
              (flycheck-mode -1))))

;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;; ELPY END

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (setq tab-width 4)
            (local-set-key [f8] 'flycheck-mode)
            (git-gutter-mode)
            (setq elpy-rpc-virtualenv-path "~/.emacs.d/elpy/rpc-venv")
            (pyvenv-workon "rpc-venv")
            ))

;; elpy settings for .dir-locals.el
;; apt install python-virtualenv
;; python3 -m venv env3
;; source env3/bin/activate
;; pip install flake8 yapf autopep8 jedi rope

;; ((nil . (
;;          (pyvenv-workon . "env3")
;;          (elpy-rpc-virtualenv-path . "~/.virtualenvs/env3")
;;          ;; (elpy-rpc-pythonpath . "~/.virtualenvs/env3/bin")
;;          (elpy-rpc-python-command . "python3")
;;          (python-shell-interpreter . "python3")
;;          (python-shell-interpreter-args . "-i")
;;          (python-indent-guess-indent-offset . 4)
;;          ))
;;  (python-mode . ((eval . (lambda ()
;;                            (git-gutter-mode))))))

;; ==========================================
;; virtualenv env2
;; source env2/bin/activate
;; pip install flake8 yapf autopep8 jedi rope

;; ((nil . (
;;          (pyvenv-workon . "env2")
;;          (elpy-rpc-virtualenv-path . "~/.virtualenvs/env2")
;;          ;; (elpy-rpc-pythonpath . "~/.virtualenvs/env2/bin")
;;          (elpy-rpc-python-command . "python2")
;;          (python-shell-interpreter . "python2")
;;          (python-shell-interpreter-args . "-i")
;;          (python-indent-guess-indent-offset . 4)
;;          ))
;;  (python-mode . ((eval . (lambda ()
;;                            (git-gutter-mode))))))

;;; emacs-rc-python.el ends here
