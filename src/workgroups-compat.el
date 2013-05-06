;; flet (Temporary redifinition) command caused a lot of warnings and is
;; obsolete
;;
;; So we need to define something new.
;; I picked up dflet.el module


(defun wg--call-with-temporary-redefinitions (function
                                               &rest function-names-and-overriding-functions)
  (let* ((overrides (remove-if-not #'(lambda (fdef)
                                       (fboundp (first fdef)))
                                   function-names-and-overriding-functions))
         (definition-names (mapcar #'first overrides))
         (overriding-functions (mapcar #'second overrides))
         (saved-functions (mapcar #'symbol-function definition-names)))
    ;; saving all definitions before overriding anything ensures FDEFINITION
    ;; errors don't cause accidental permanent redefinitions.
    ;;
    ;;(labels ((set-fdefinitions (names functions)
    (cl-labels ((set-fdefinitions (names functions)
                               (loop for name in names
                                     for fn in functions
                                     do (fset name fn))))
      (set-fdefinitions definition-names overriding-functions)
      (unwind-protect (funcall function)
        (set-fdefinitions definition-names saved-functions)))))


(defmacro wg--with-temporary-redefinitions (fdefinitions &rest body)
  ;; "Temporarily (but globally) redefine each function in FDEFINITIONS.
  ;; E.g.: (wg--with-temporary-redefinitions ((foo (x) ...)
  ;; (bar (x) ...))
  ;; ;; code that eventually calls foo, bar of (setf foo)
  ;; ...)"
  `(wg--call-with-temporary-redefinitions
    (lambda () ,@body)
    ,@(mapcar #'(lambda (thingy)
                  `(list ',(first thingy)
                         (lambda ,@(rest thingy))))
              fdefinitions)))

(put 'wg--with-temporary-redefinitions 'lisp-indent-function 1)
(put 'wg--with-temporary-redefinitions 'edebug-form-spec '((&rest (defun*)) cl-declarations body))


(provide 'workgroups-compat)
