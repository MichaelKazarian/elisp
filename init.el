;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/elisp/mode")
(add-to-list 'load-path "~/elisp/rc")

;; У :packages лише явно потрібні пакети. Залежності розгортаються автоматично.
(defconst my/profiles
  '((base   :parent nil
            :packages (markdown-mode sunny-day-theme git-gutter popwin
                                     marginalia kaolin-themes)
            :rc ("editing" "screen" "rc-mouse" "rc-completion"
                 "markdown" "rc-org" "backup" "savehistory" "kbd"))
    (reader :parent base
            :packages (nov)
            :rc ("rc-nov" "rc-pdf-text" "rc-reader"))
    (dev    :parent base
            :packages (magit elpy company company-quickhelp ahg helpful
                             highlight-symbol imenu-list yasnippet indent-guide)
            :rc ("vcs" "rc-prog" "lisp" "tags" "rc-clang" "rc-eshell"
                 "rc-nxml" "rc-json" "rc-python" "java" "rc-completion-dev"
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

(defun my/package-closure (roots)
  "Return ROOTS plus all their transitive dependencies from `package-alist'."
  (let ((seen nil) (queue (copy-sequence roots)))
    (while queue
      (let ((name (pop queue)))
        (unless (memq name seen)
          (push name seen)
          (if-let* ((desc (cadr (assq name package-alist))))
              (dolist (req (package-desc-reqs desc))
                (push (car req) queue))
            (unless (or (eq name 'emacs) (package-built-in-p name))
              (message "Package %s is not installed" name))))))
    seen))

;; Packages: activate only the profile's packages and their dependencies
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize t)                  ; only read descriptors, no activation

;; Activate profile packages and dependencies selectively to avoid marking
;; packages as incompatible
(mapc #'package-activate
      (my/package-closure (my/profile-collect :packages)))

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

;; For `package-autoremove': the union of packages of ALL profiles,
;; so cleaning from one profile does not remove another profile's packages.
(setq package-selected-packages
      (delete-dups
       (apply #'append
              (mapcar (lambda (p) (copy-sequence (plist-get (cdr p) :packages)))
                      my/profiles))))
