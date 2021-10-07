;;; emacs-rc-tags.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

;;; TAGS file update after save start
(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
 Returns the path to it or nil if not found."
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

;; (add-hook 'after-save-hook 'ctags-update)
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
