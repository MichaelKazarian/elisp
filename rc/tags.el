;;; emacs-rc-tags.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

;; ;; ac-etags start. It is etags/ctags completion source for auto-complete. 
;; (custom-set-variables
;;   '(ac-etags-requires 1))

;; (eval-after-load "etags"
;;   '(progn
;;       (ac-etags-setup)))

;; (add-hook 'c-mode-common-hook 'ac-etags-ac-setup)
;; (add-hook 'python-mode-hook 'ac-etags-ac-setup)
;; (add-hook 'java-mode-hook 'ac-etags-ac-setup)
;; (add-hook 'jde-mode-hook 'ac-etags-ac-setup)
;; (add-hook 'nxml-mode-hook 'ac-etags-ac-setup)

;; (defun ac-etags-visit-table ()
;;   (let ((tags-file (find-file-upwards "TAGS")))
;;     (when tags-file
;;       (visit-tags-table tags-file)
;;       (message tags-file))))
;; (add-hook 'python-mode-hook 'ac-etags-visit-table)
;; (add-hook 'java-mode-hook 'ac-etags-visit-table)
;; ;; ac-etags stop

;; ;; (require 'auto-complete-exuberant-ctags)
;; ;; (add-hook 'c-mode-common-hook 'ac-exuberant-ctags-setup)
;; ;; (add-hook 'python-mode-hook 'ac-exuberant-ctags-setup)
;; ;; (add-hook 'java-mode-hook 'ac-exuberant-ctags-setup)
;; ;; (add-hook 'jde-mode-hook 'ac-exuberant-ctags-setup)
;; ;; (add-hook 'nxml-mode-hook 'ac-exuberant-ctags-setup)

;;; TAGS file update after save start
(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (locate-dominating-file default-directory file-to-find))

(defun ctags-update ()
  (interactive)
  (let ((tags-file (expand-file-name (find-file-upwards "TAGS"))))
    (when tags-file
      (setq tags-file (concat tags-file "TAGS"))
      (message tags-file)
      (message (file-name-directory tags-file))
      (call-process "ctags-universal"  nil nil nil
               "-f" tags-file "-R" "-e"
               " --exclude='*.elc'"
               " --exclude='*.pyc'"
               " --exclude='*.class'"
               " --exclude='*.jar'"
               " --exclude='.git'"
               " --exclude='.hg'"
               " --exclude='.svn'"
               (file-name-directory tags-file)))))

;(add-hook 'after-save-hook 'ctags-update)
;; (add-hook 'python-mode-hook 
;;           (lambda () 
;;             (add-hook 'after-save-hook 'ctags-update nil 'make-it-local)))

;;; TAGS file update after save stop

;; ;;; GNU Global project complete start

;; ;; GNU GLOBAL incremental update http://www.emacswiki.org/emacs/GnuGlobal

;; (defun gtags-root-dir ()
;;    "Returns GTAGS root directory or nil if doesn't exist."
;;     (with-temp-buffer
;;       (if (zerop (call-process "global" nil t nil "-pr"))
;;           (buffer-substring (point-min) (1- (point-max)))
;;         nil)))
;; (defun gtags-update ()
;;     "Make GTAGS incremental update"
;;     (call-process "global" nil nil nil "-u"))
;; (defun gtags-update-incrementally-hook ()
;;     (when (gtags-root-dir)
;;       (gtags-update)))
;; (add-hook 'after-save-hook #'gtags-update-incrementally-hook)

;; ;; GNU Global project complete end

;; emacs-rc-tags.el ends here
