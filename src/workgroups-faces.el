;;; workgroups-faces.el --- Colors
;;; Commentary:
;; display customization
;;; Code:

(defcustom wg-use-faces t
  "Non-nil means use faces in various messages."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-list-display-decor-left-brace "( "
  "String displayed to the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-right-brace " )"
  "String displayed to the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-divider " | "
  "String displayed between elements of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-current-left "-<{ "
  "String displayed to the left of the current element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-current-right " }>-"
  "String displayed to the right of the current element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-previous-left "< "
  "String displayed to the left of the previous element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-previous-right " >"
  "String displayed to the right of the previous element of the list display."
  :type 'string
  :group 'workgroups)


(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")

(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (cl-pushnew (cons ,key ',face) wg-face-abbrevs :test #'equal)
     (defface ,face ,spec ,doc ,@args)))

(wg-defface wg-current-workgroup-face :cur
  '((t :inherit font-lock-constant-face :bold nil))
  "Face used for current elements in list displays."
  :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for the name of the previous workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for the names of other workgroups in the list display."
  :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((t :inherit font-lock-function-name-face :bold nil))
  "Face used for command/operation strings."
  :group 'workgroups)

(wg-defface wg-divider-face :div
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for dividers."
  :group 'workgroups)

(wg-defface wg-brace-face :brace
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for left and right braces."
  :group 'workgroups)

(wg-defface wg-message-face :msg
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for messages."
  :group 'workgroups)

(wg-defface wg-mode-line-face :mode
  '((t :inherit font-lock-doc-face :bold nil))
  "Face used for workgroup position and name in the mode-line display."
  :group 'workgroups)

(wg-defface wg-filename-face :file
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for filenames."
  :group 'workgroups)


;;; fancy displays

(defun wg-element-display (elt elt-string &optional current-elt-p previous-elt-p)
  "Return display string for ELT."
  (cond ((and current-elt-p (funcall current-elt-p elt))
         (wg-fontify (:cur (concat wg-list-display-decor-current-left
                                   elt-string
                                   wg-list-display-decor-current-right))))
        ((and previous-elt-p (funcall previous-elt-p elt))
         (wg-fontify (:prev (concat wg-list-display-decor-previous-left
                                    elt-string
                                    wg-list-display-decor-previous-right))))
        (t (wg-fontify (:other elt-string)))))

(defun wg-workgroup-display (workgroup index)
  "Return display string for WORKGROUP at INDEX."
  (if (not workgroup) wg-nowg-string
    (wg-element-display
     workgroup
     (format "%d: %s" index (wg-workgroup-name workgroup))
     'wg-current-workgroup-p
     'wg-previous-workgroup-p)))

(defun wg-buffer-display (buffer index)
  "Return display string for BUFFER.  INDEX is ignored."
  (if (not buffer) "No buffers"
    (wg-element-display
     (wg-get-buffer buffer)
     (format "%s" (wg-buffer-name buffer))
     'wg-current-buffer-p)))


;;; messaging

(defun wg-message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS.
Also save the msg to `wg-last-message'."
  (setq wg-last-message (apply #'message format-string args)))

(defmacro wg-fontified-message (&rest format)
  "`wg-fontify' FORMAT and call `wg-message' on it."
  (declare (indent defun))
  `(wg-message (wg-fontify ,@format)))

(defun wg-toggle-and-message (symbol)
  "Toggle SYMBOL's truthiness and message the new value."
  (wg-fontified-message
    (:cmd (format "%s: " symbol))
    (:msg (format "%s" (wg-toggle symbol)))))


(defun wg-add-face (facekey string)
  "Return a copy of STRING fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-aget wg-face-abbrevs facekey))
        (string  (copy-sequence string)))
    (unless face (error "No face with key %s" facekey))
    (if (not wg-use-faces) string
      (put-text-property 0 (length string) 'face face string)
      string)))

(defmacro wg-fontify (&rest specs)
  "A small fontification DSL.
The results of all SPECS are `concat'd together.
If a spec is a cons with a keyword car, apply `wg-add-face' to it.
Other conses get evaluated, and should produce a strings.
If a spec is a string it is used unmodified.
Anything else is formatted with %s to produce a string."
  (declare (indent defun))
  `(concat
    ,@(wg-docar (spec specs)
        (cond ((and (consp spec)
                    (keywordp (car spec)))
               `(wg-add-face ,@spec))
              ;;((listp spec) (cdr (eval spec)))
              ;;((listp spec)
              ;; ;;(prin1-to-string (nth 1 (eval spec)))
              ;; )
              ((consp spec) spec)
              ((stringp spec) spec)
              (t `(format "%s" ,spec))))))

(provide 'workgroups-faces)
;;; workgroups-faces.el ends here
