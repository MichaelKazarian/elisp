## Fix cl-return error when processing grouped Imenu tags

### Environment

- ECB: 2.52
- GNU Emacs: 30.2
- Major mode: go-mode, org-mode, markdown
- Parsing backend: Imenu via speedbar-fetch-dynamic-imenu

### Problem

When ECB processes a non-semantic file whose Imenu index already contains groups, the Methods buffer remains empty.

In my case, `speedbar-fetch-dynamic-imenu` correctly returns a grouped index containing func and type entries. However, `ecb-get-tags-for-non-semantic-files signals` from `ebc-speedbar.el` the following error:
```
Debugger entered--Lisp error: (no-catch --cl-block-nil-- nil)
  throw(--cl-block-nil-- nil)
  ecb-get-tags-for-non-semantic-files()
```
The error occurs when `cl-return` is executed inside the dolist form that checks whether the tags already contain groups.

The error is subsequently suppressed by `ignore-errors` in the caller, so the Methods buffer remains empty without displaying the underlying exception.

### Proposed fix

Replace the `dolist/cl-return` check with `seq-some`. This preserves the intended behavior: if the tags already contain groups, do not apply the additional hierarchy transformations.

The replacement fixes the issue in my setup, and ECB correctly displays the Imenu groups afterward.

### Patch
```elisp
-      (when (dolist (tag tag-list t)
-              (if (or (speedbar-generic-list-positioned-group-p tag)
-                      (speedbar-generic-list-group-p tag))
-                  (cl-return nil)))
+      (unless (seq-some
+               (lambda (tag)
+                 (or (speedbar-generic-list-positioned-group-p tag)
+                     (speedbar-generic-list-group-p tag)))
+               tag-list)
         (while methods
           (setq tag-list (funcall (car methods) tag-list)
                 methods (cdr methods))))
```
Tested with `go-mode`, `org-mode`, and `Markdown`. In all three cases, the fix allows ECB to display the grouped Imenu entries correctly.
