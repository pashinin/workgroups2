;;; workgroups-compat --- some functions for different versions of Emacs
;;; Commentary:
;; flet (Temporary redifinition) command caused a lot of warnings and is
;; obsolete
;;
;; So we need to define something new.
;; I picked up dflet.el module

;;; Code:

;; Emacs 24.3+, use "cl-labels" instead of "labels"
(if (version< emacs-version "24.3")
    (progn
      (defalias 'wg-gensym 'gensym)
      (defalias 'wg-position 'position)
      (defalias 'wg-remove-if-not 'remove-if-not)
      (defalias 'wg-labels 'labels))
  (progn
    (defalias 'wg-gensym 'cl-gensym)
    (defalias 'wg-position 'cl-position)
    (defalias 'wg-remove-if-not 'cl-remove-if-not)
    (defalias 'wg-labels 'cl-labels)))


(provide 'workgroups-compat)
;;; workgroups-compat.el ends here
