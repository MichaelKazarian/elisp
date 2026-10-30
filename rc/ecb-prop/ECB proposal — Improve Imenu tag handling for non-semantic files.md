# Improve Imenu tag handling for non-semantic files

I would like to propose an improvement to `ecb-get-tags-for-non-semantic-files` in ECB.

## Problem

ECB Methods does not always display Imenu entries correctly for non-semantic files.

Speedbar can return either a flat list of tags or a list that already contains groups. These structures need to be handled differently:

- Flat tag lists need a synthetic root group so that ECB can display them correctly.
- Lists that already contain groups should retain their original hierarchy.

Applying hierarchy transformations to an already grouped list can result in unnecessary or incorrect nesting.

There is also an issue with the original group detection approach: using `dolist` with `cl-return` can raise a `no-catch` error in this context.

## Proposed implementation

I suggest replacing `ecb-get-tags-for-non-semantic-files` with the following implementation:

```elisp
(defun ecb-speedbar-tag-list-flat-p (tags)
  "Return non-nil if TAGS contains no Speedbar groups."
  (and tags
       (not (seq-some
             (lambda (tag)
               (or (speedbar-generic-list-positioned-group-p tag)
                   (speedbar-generic-list-group-p tag)))
             tags))))

(defun ecb-get-tags-for-non-semantic-files ()
  "Get a tag-list for current source-file via Speedbar."
  (require 'imenu)
  (if (member major-mode ecb-non-semantic-exclude-modes)
      nil
    (let* ((lst
            (let ((speedbar-dynamic-tags-function-list
                   (if (not (assoc major-mode
                                   ecb-non-semantic-parsing-function))
                       (list (cons 'speedbar-fetch-dynamic-imenu 'identity)
                             (cons 'speedbar-fetch-dynamic-etags 'identity))
                     (list (cons (cdr (assoc major-mode
                                             ecb-non-semantic-parsing-function))
                                 'identity)))))
              (speedbar-fetch-dynamic-tags
               (ecb-buffer-file-name (current-buffer)))))
           (tag-list (cdr lst))
           (methods speedbar-tag-hierarchy-method))

      ;; Remove Imenu rescan item.
      (when (and tag-list
                 (ecb-string= (caar tag-list)
                              (car imenu--rescan-item)))
        (setq tag-list (cdr tag-list)))

      ;; Wrap flat tag lists in a synthetic root group.
      (when (ecb-speedbar-tag-list-flat-p tag-list)
        (setq tag-list
              (list (cons "root" tag-list))))

      ;; Do not regroup existing groups.
      (unless (seq-some
               (lambda (tag)
                 (or (speedbar-generic-list-positioned-group-p tag)
                     (speedbar-generic-list-group-p tag)))
               tag-list)
        (while methods
          (setq tag-list (funcall (car methods) tag-list)
                methods (cdr methods))))

      tag-list)))
```

## Benefits

- Correctly handles flat Imenu tag lists.
- Preserves existing Speedbar groups and their hierarchy.
- Avoids unnecessary regrouping of already structured tag lists.
- Uses `seq-some` for group detection instead of the problematic `dolist` / `cl-return` approach.
- Safely handles an empty tag list when removing the Imenu rescan entry.

## Testing

I have tested this implementation with several file types, including:

- Go
- Java
- C
- C header files
- Emacs Lisp
- Org
- Markdown

It works with both flat tag lists and formats that already provide a hierarchy.

A working example, including the related ECB configuration, is available in the attached `rc-ecb.el` file.

Tested with ECB 2.52.