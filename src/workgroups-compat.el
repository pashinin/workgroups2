;;; workgroups-compat --- some functions for different versions of Emacs
;;; Commentary:
;; flet (Temporary redifinition) command caused a lot of warnings and is
;; obsolete
;;
;; So we need to define something new.
;; I picked up dflet.el module

;;; Code:

(require 'macroexp)

(or (fboundp 'declare-function)
    (defmacro declare-function (&rest args)
      nil))

(if (version< emacs-version "24.3")
    ;; sure it doesn't exist, but it won't be called anyway...
    (autoload 'cl--compiling-file "cl")
  (declare-function cl--compiling-file "cl" t t))

(if (version< emacs-version "24.3")
    ;; before that version, flet was not marked as obsolete, so use it
    (defalias 'dflet 'flet)

  ;; This should really have some way to shadow 'byte-compile properties, etc.
  (defmacro dflet (bindings &rest body)
    "Make temporary overriding function definitions.
This is an analogue of a dynamically scoped `let' that operates on the function
cell of FUNCs rather than their value cell.
If you want the Common-Lisp style of `flet', you should use `cl-flet'.
The FORMs are evaluated with the specified function definitions in place,
then the definitions are undone (the FUNCs go back to their previous
definitions, or lack thereof).

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
    (declare (indent 1) (debug cl-flet))
    `(cl-letf ,(mapcar
             (lambda (x)
               (if (or (and (fboundp (car x))
                            (eq (car-safe (symbol-function (car x))) 'macro))
                       (cdr (assq (car x) macroexpand-all-environment)))
                   (error "Use `labels', not `dflet', to rebind macro names"))
               (let ((func `(cl-function
                             (lambda ,(cadr x)
                               (cl-block ,(car x) ,@(cddr x))))))
                 (when (cl--compiling-file)
                   ;; Bug#411.  It would be nice to fix this.
                   (and (get (car x) 'byte-compile)
                        (error "Byte-compiling a redefinition of `%s' \
will not work - use `labels' instead" (symbol-name (car x))))
                   ;; FIXME This affects the rest of the file, when it
                   ;; should be restricted to the flet body.
                   (and (boundp 'byte-compile-function-environment)
                        (push (cons (car x) (eval func))
                              byte-compile-function-environment)))
                 (list `(symbol-function ',(car x)) func)))
             bindings)
       ,@body)))

;;;###autoload
(autoload 'dflet "dflet")



(provide 'workgroups-compat)
;;; workgroups-compat.el ends here
