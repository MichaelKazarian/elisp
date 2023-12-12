;;; emacs-rc-tags.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(setq tags-revert-without-query 1)

;;; TAGS file update after save start
(defun get-etags-command ()
  (let ((snap-etags "/snap/emacs/current/usr/bin/etags"))
    (if (file-exists-p snap-etags) snap-etags
      "/usr/bin/etags.emacs")))

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
 Returns the path to it or nil if not found."
  (locate-dominating-file default-directory file-to-find))

(defun call-etags-universal (tags-file)
  "Calls ctags-universal to create/update TAGS file"
  (call-process "ctags-universal"  nil nil nil
                "-f" tags-file "-R" "-e"
                " --exclude='*.elc'"
                " --exclude='*.pyc'"
                " --exclude='*.class'"
                " --exclude='*.jar'"
                " --exclude='.git'"
                " --exclude='.hg'"
                " --exclude='.svn'"
                (file-name-directory tags-file)))

(defun call-etags-create (tags-file)
  "Calls emacs built-in ctags to create TAGS file"
  (call-process "/bin/bash"
                nil nil nil
                "-c"
                (concat "find . "
                        " -type f \\( -iname '*.c' -o -iname '*.h*'"
                        " -o -iname '*.cpp'"
                        " -o -iname '*.py'"
                        " -o -iname '*.el'"
                        " ! -iname '*vendor*' \\) -print"
                        " | "
                        (get-etags-command)
                        " -o " tags-file " -")))

(defun etags-tag-create ()
  "Create TAGS file in the current directory using emacs built-in ctags."
  (interactive)
  (let ((tags-file (expand-file-name "TAGS")))
    ;; (call-etags-universal tags-file)
    (call-etags-create tags-file)))

(defun call-etags-update (tags-file)
  "Calls emacs ctags to update tags-file"
  (call-process "/bin/bash"
                nil nil nil
                (concat (get-etags-command) " -a -o "
                        tags-file " " default-directory "*.*")))

(defun etags-update ()
  "Searches TAGS file in the upward directories starting from current and update
 if found."
  (interactive)
  (let ((tags-file (expand-file-name (find-file-upwards "TAGS"))))
    (when tags-file
      (setq tags-file (concat tags-file "TAGS"))
      ;; (call-ctags-universal tags-file)
      (call-etags-update tags-file)
      (message (get-etags-command))
      )))

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
