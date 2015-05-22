;;; narrow-reindent.el
;;;
;;; Summary:
;;;
;;; Defines a minor mode `narrow-reindent-mode' that reindents the region
;;; narrowed to after calling `narrow-to-defun'. The region is then indented
;;; again after widening the buffer.
;;;
;;; Code:

(defvar-local narrow-reindent--point-min 0)
(defvar-local narrow-reindent--point-max 0)

(define-minor-mode narrow-reindent-mode
  "Toggle Narrow-Reindent mode.

When Narrow-Reindent mode is active, after narrowing to a defun
the buffer is re-indented. After widening, this narrowed region
is re-indented again. This mode uses the `indent-region' to
perform indentation."
  :lighter " NaRe"
  :group 'narrow-reindent
  :init-value nil
  (if narrow-reindent-mode
      (progn
        (advice-add #'narrow-to-defun :after #'narrow-reindent--after-narrow)
        (advice-add #'widen :after #'narrow-reindent--after-widen))
    (progn
      (advice-remove #'narrow-to-defun #'narrow-reindent--after-narrow)
      (advice-remove #'widen #'narrow-reindent--after-widen))))

(defmacro without-undo (&rest forms)
  "Executes FORMS with a temporary buffer-undo-list that is discarded afterwards.

Taken from http://www.emacswiki.org/emacs/UndoCommands with some
modifications."
`(let* ((buffer-undo-list)
          (modified (buffer-modified-p))
          (inhibit-read-only t))
   (unwind-protect
       (progn ,@forms)
     (set-buffer-modified-p modified)) ()))

(defun narrow-reindent--after-narrow ()
  "Indent narrowed buffer. This function is used as advice for
`narrow-to-defun'."
  (let ((beg (point-min))
        (end (point-max)))
    (setq narrow-reindent--point-min beg)
    (setq narrow-reindent--point-max end)
    (without-undo
     (indent-region beg end))))

(defun narrow-reindent--after-widen ()
  "Indent the region that the buffer was narrowed to. This
function is used as advice for `widen'."
  (without-undo
   (indent-region narrow-reindent--point-min narrow-reindent--point-max)))


(provide 'narrow-reindent)
;;; narrow-reindent.el ends here
