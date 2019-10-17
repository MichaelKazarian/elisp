;;; lesson-org-mode.el --- 

;; Copyright (C) Michael Kazarian
;;
;; Author: Michael Kazarian <michael.kazarian@gmail.com>
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet
;; Some ideas found here
;; https://github.com/defunkt/coffee-mode/blob/master/coffee-mode.el

;;
;; Customizable Variables
;;
(defconst lesson-mode-version "0.1"
  "The version of `lesson-mode'.")

(defcustom message-time-delay 6
  "Delay time before answer hint."
  :type 'integer
  :safe 'integerp
  :group 'lesson-mode)

(defcustom word-preview-time 3
  "Word preview time"
  :type 'integer
  :safe 'integerp
  :group 'lesson-mode)

(defcustom answer-question-delimiter ";"
  "Question-answer delimiter. Answer is first."
  :type 'string
  :group 'lesson-mode)

;;
;; Core
;;
(defun lang-delimiter-position (str)
  "Returns `answer-question-delimiter' position if presents; nil otherwise"
  (string-match answer-question-delimiter str))

(defun answer-message ()
  "Hint to say answer and publish it in the slide"
  (setq str (buffer-substring
           (line-beginning-position)
           (line-end-position)))
  (message (format "ANSWER: ~ %s ~ and F5 to publish it" str)))

(defun new-q-message ()
  "Say Ukrainian text"
  (let* ((str (buffer-substring (line-beginning-position) (line-end-position)))
         (delim (lang-delimiter-position str)))
    (if delim (setq str (substring str delim)))
     (message (format "TO UKR: ~ %s ~ " str)))
  ;; (setq str (buffer-substring
  ;;          (line-beginning-position)
  ;;          (line-end-position)))
  ;;  (message (format "TO UKR: ~ %s ~ " str))
 )

(defun lesson-switch-to-lesson ()
  "Switch to lesson window. After switching searche next question by
number. If question found send message after `message-time-delay' sec."
  (interactive)
  (other-window 1)
  (let ((start-point (re-search-forward "^[[:space:]]*[0-9]" nil t)))
    (if start-point
        (progn
          (goto-char start-point)
          ;; (new-q-message)
          (run-at-time message-time-delay nil #'answer-message))
      (next-line))))

(defun lesson-send-to-slide ()
  "Send current line to slide buffer and switch to it"
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil)
  (let* ((str (buffer-substring (region-beginning) (region-end)))
         (str-end (lang-delimiter-position str))
         (str-start (- (region-beginning) 1)))
    (if (not str-end)
        (setq str-end (region-end))
      (setq str-end (+ str-start str-end 1)))
    (append-to-buffer "slide" str-start str-end))
  ;; (append-to-buffer "slide"
  ;;                   (- (region-beginning) 1)
  ;;                   (region-end))

  (deactivate-mark)
  (lesson-switch-to-lesson)
  (next-window 1))

(defun word-region-blink-to-slide ()
  "Temporary send current word or region to slide buffer and switch to it.
Clear this line after `word-preview-time and switch to previous buffer."
  (interactive)
  (let ((str (word-at-point)))
    (if (region-active-p)
            (setq str (buffer-substring (region-beginning) (region-end))))
    (with-current-buffer "slide"
      (progn
        (message (region-active-p))
        (insert (concat "\n~" (string-trim str) "~"))
        (end-of-line)
        (sit-for word-preview-time)
        (clear-line-go-to-lesson)
        (other-window 1)
        ))))

(defun clear-line-go-to-lesson ()
  "Clear current line and return to previous window"
  (interactive)
  (kill-whole-line)
  (kill-backward-chars 1)
  (other-window 1))

(defun lesson-slide-swich ()
  "If lesson buffer is active send current line to slide.
If slide buffer is active switch to lesson buffer an search new question"
  (interactive)
  (if (equal "slide" (buffer-name))
      (lesson-switch-to-lesson)
    (lesson-send-to-slide)))

(defun lesson-new-slide ()
  "If no slide buffer found create new one. Clear it otherwise"
  (interactive)
  (let ((slide-name "slide"))
    (if (get-buffer slide-name)
        (with-current-buffer slide-name
          (erase-buffer))
      (progn
        (generate-new-buffer slide-name)
        (with-current-buffer slide-name
          (funcall 'lesson-mode)
          (text-scale-set 3)
          (split-window-right)
          (switch-to-buffer-other-window slide-name)
          (enlarge-window-horizontally 35)
          (other-window 1)
          ;(text-scale-adjust 1)
          )))))

(defun s ()
  "Setup `org-mode' for lesson"
  (set-face-background 'hl-line "NavajoWhite")
  (set-face-attribute hl-line-face nil :underline nil)
  (linum-mode -1)
  (olivetti-mode -1)
  (setq org-hide-emphasis-markers t)
  (load-theme 'dichromacy t)

  ;; make part of a word bold
  ;; https://stackoverflow.com/posts/24540651/revisions
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;;
  (set-frame-size (selected-frame) 150 60)
  (set-face-attribute 'org-level-2 nil :foreground "DarkGreen")
  (add-to-list 'org-emphasis-alist
             '("~" (:foreground "OrangeRed"))))
;;
;; orgl to json converter
;;
(defun lesson-to-json ()
  "Convert lesson to json. If line begining is numbered item line will convert
to json question item (use ; to split question and answer). All other strings
will convert to json point type."
  (interactive)
  (let ((from-file (read-file-name "To json:" (buffer-file-name)))
         (to-file (read-file-name "Save to:" nil nil nil (concat (file-name-sans-extension (buffer-name)) ".json"))))
         (message to-file)
         (setq input-lines (read-lesson from-file))
         (write-json input-lines to-file)
         (find-file to-file)))

(defun read-lesson (file)
  "Read `file'.
Returns list of strings"
  (split-string
          (with-temp-buffer
           (insert-file-contents file)
           (buffer-substring-no-properties
            (point-min)
            (point-max)))
          "\n" t))

(defun write-json (lines file)
  "Writes `lines' to `file'"
  (setq header "{
  \"lessonId\": \"lesson\",
  \"lessonTitle\": \"Урок \",
  \"lessonDescription\": \" \",
  \"lessonURL\": \"bJkSM3g-Tfs\",
  \"lessonItems\": [")
  (setq footer "\n  ]\n}\n")
  (with-temp-file file
    (insert header)
    (dolist (line lines)
      (setq l (replace-org-to-html line))
      (if (string-match "^[[:space:]]*[[:digit:]]\\{1,5\\}\\. " l)
          (setq item (get-question l))
        (setq item (get-point l)))
      (insert item))
    (insert footer)))

(defun get-question (str)
  "Returns prepared json question item. If `str' contains
`answer-question-delimiter' first part will answer, the second one will
question. If delimiter omited question part will empty"
  (setq template "\n    {
      \"itemType\": \"question\",
      \"question\": \"%s\",
      \"answer\":   \"%s\",
      \"startTime\": \":\"
    },")
  (format template
   (get-question-part str)
   (get-answer-part str)))

(defun get-point (str)
  "Returns prepared json point item"
  (setq template "\n    {
      \"itemType\": \"point\",
      \"itemText\": \"%s\",
      \"startTime\": \":\"
    },")
  (format template str))

(defun replace-org-to-html (str)
  "Relaces /*_ org formating to <i></i>, <b></b>, <u></u> accordingly"
  (let* ((pattern "[/_\\*]\\(.+\\)[/_\\*]")
         (found (string-match pattern str)))
    (if (null found)
        str
      (progn
        (setq tag (substring str found (+ found 1)))
        (message tag)
        (setq t-s "") (setq t-e "")
        (cond ((equal "*" tag) (setq t-s "<b>") (setq t-e "</b>"))
              ((equal "_" tag) (setq t-s "<u>") (setq t-e "</u>"))
              ((equal "/" tag) (setq t-s "<i>") (setq t-e "</i>")))
        (mapconcat
         'identity (split-string str pattern)
         (concat t-s (match-string 1 str) t-e)))
      )))

(defun get-answer-part (line)
  "If `line' contains `answer-question-delimiter' first part will return.
Whole line otherwise"
  (let ((delimiter (lang-delimiter-position line)))
    (if (null delimiter)
        line
      (string-trim (substring line 0 delimiter)))))

(defun get-question-part (line)
  "If `line' contains `answer-question-delimiter' the second part will return.
Empty line otherwise"
  (let ((delimiter (lang-delimiter-position line)))
    (if (null delimiter)
        ""
      (string-trim (substring line (+ 1 delimiter))))))
;;
;; Mode settings, etc
;;
(defun lesson-mode-version ()
  "Show the `lesson-mode' version in the echo area."
  (interactive)
  (message (concat "lesson-mode version " lesson-mode-version)))

(defvar lesson-mode-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "<f5>") 'lesson-slide-swich)
    (define-key map (kbd "<f6>") 'word-region-blink-to-slide)
    (define-key map (kbd "<f8>") 'clear-line-go-to-lesson)
    (define-key map (kbd "<f9>") 'lesson-new-slide)
    map)
  "Keymap for `lesson-mode' major mode.")

(easy-menu-define lesson-mode-menu lesson-mode-map
  "Menu for `lesson-mode' mode"
  '("Lesson"
    ["New slide" lesson-new-slide]
    ["Send line to slide" lesson-slide-swich]
    ["Word or region blink to slide" word-region-blink-to-slide]
    "---"
    ["Version" lesson-mode-version]))

(define-derived-mode lesson-mode org-mode "Lesson"
  "Major mode for English lesson"
  (use-local-map lesson-mode-map))

(add-hook 'lesson-mode-hook 's)
(add-hook 'lesson-mode-hook 'flyspell-mode)
(provide 'lesson-mode)

;;
;; On Load
;;
;; Run lesson-mode for files ending in .orgl or .lesson
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lesson\\'" . lesson-mode))
(add-to-list 'auto-mode-alist '("\\.orgl\\'" . lesson-mode))

;; ;; Old
;; (defun lesson-new-slide-temp ()
;;   (interactive)
;;   (let ((slide-name "slide"))
;;     (if (get-buffer slide-name)
;;         (progn
;;           (kill-buffer slide-name)
;;           (delete-other-windows)
;;           ))
;;     (generate-new-buffer slide-name)
;;     (with-current-buffer slide-name
;;       (funcall 'lesson-mode))
;;     (split-window-right)
;;     (switch-to-buffer-other-window slide-name)))

;; (defun lesson-clear-order-mark ()
;;   (interactive)
;;   (previous-line)
;;   (while (re-search-forward " \\[@[[:digit:]{1,4}]\\] " nil t)
;;     (replace-match " ")))

;; ;; Another send-to slide solution
;; (setq str (buffer-substring
;;            (region-beginning)
;;            (region-end)))
;; (with-current-buffer "slide"
;;   (insert (concat str "\n")))

;;; lesson-org-mode.el ends here
