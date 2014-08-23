;;; workgroups-utils.el --- Utilities used by Workgroups
;;; Commentary:
;;
;; A bunch of general purpose-ish utilities used by Workgroups.
;;
;;; Code:

;;; utils used in macros

(require 'dash)
(require 'cl-lib)
(require 'anaphora)
(require 'workgroups-faces)
(require 'workgroups-variables)

(defmacro wg-with-gensyms (syms &rest body)
  "Bind all symbols in SYMS to `gensym's, and eval BODY."
  (declare (indent 1))
  `(let (,@(mapcar (lambda (sym) `(,sym (cl-gensym))) syms)) ,@body))

(defmacro wg-dbind (args expr &rest body)
  "Bind the variables in ARGS to the result of EXPR and execute BODY.
Abbreviation of `destructuring-bind'."
  (declare (indent 2))
  `(cl-destructuring-bind ,args ,expr ,@body))

(defun wg-partition (list &optional n step)
  "Take LIST, return a list of N length sublists, offset by STEP.
N defaults to 2, and STEP defaults to N.
Iterative to prevent stack overflow."
  (let* ((n (or n 2)) (step (or step n)) acc)
    (while list
      (push (-take n list) acc)
      (setq list (nthcdr step list)))
    (nreverse acc)))



;;; bindings

(defmacro wg-when-boundp (symbols &rest body)
  "When all SYMBOLS are bound, `eval' BODY."
  (declare (indent 1))
  `(when (and ,@(mapcar (lambda (sym) `(boundp ',sym)) symbols))
     ,@body))



;;; do-style wrappers

(defmacro wg-docar (spec &rest body)
  "do-style wrapper for `mapcar'.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defmacro wg-dohash (spec &rest body)
  "do-style wrapper for `maphash'.

\(fn (KEY VALUE TABLE [RESULT]) BODY...)"
  (declare (indent 1))
  (wg-dbind (key val table &optional result) spec
    `(progn (maphash (lambda (,key ,val) ,@body) ,table) ,result)))

(defmacro wg-doconcat (spec &rest body)
  "do-style wrapper for `mapconcat'.

\(fn (VAR SEQ [SEPARATOR]) BODY...)"
  (declare (indent 1))
  (wg-dbind (elt seq &optional sep) spec
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))



;;; anaphora

(defmacro wg-asetf (&rest places-and-values)
  "Anaphoric `setf'."
  `(progn ,@(mapcar (lambda (pv) `(let ((it ,(car pv))) (setf ,@pv)))
                    (wg-partition places-and-values 2))))



;;; other control structures

(defmacro wg-destructuring-dolist (spec &rest body)
  "Loop over a list.
Evaluate BODY, destructuring LIST into SPEC, then evaluate RESULT
to get a return value, defaulting to nil.  The only hitch is that
spec must end in dotted style, collecting the rest of the list
into a var, like so: (a (b c) . rest)

\(fn (SPEC LIST [RESULT]) BODY...)"
  (declare (indent 1))
  (wg-dbind (loopspec list &optional result) spec
    (let ((rest (cdr (last loopspec))))
      (wg-with-gensyms (list-sym)
        `(let ((,list-sym ,list))
           (while ,list-sym
             (wg-dbind ,loopspec ,list-sym
               ,@body
               (setq ,list-sym ,rest)))
           ,result)))))


;;; numbers

(defun wg-step-to (n m step)
  "Increment or decrement N toward M by STEP.
Return M when the difference between N and M is less than STEP."
  (cond ((= n m) n)
        ((< n m) (min (+ n step) m))
        ((> n m) (max (- n step) m))))

(defun wg-within (num lo hi &optional hi-inclusive)
  "Return t when NUM is within bounds LO and HI.
HI-INCLUSIVE non-nil means the HI bound is inclusive."
  (and (>= num lo) (if hi-inclusive (<= num hi) (< num hi))))

(defun wg-int-to-b36-one-digit (i)
  "Return a character in 0..9 or A..Z from I, and integer 0<=I<32.
Cribbed from `org-id-int-to-b36-one-digit'."
  (cond ((not (wg-within i 0 36))
         (error "%s out of range" i))
        ((< i 10) (+ ?0 i))
        ((< i 36) (+ ?A i -10))))

(defun wg-b36-to-int-one-digit (i)
  "Turn a character 0..9, A..Z, a..z into a number 0..61.
The input I may be a character, or a single-letter string.
Cribbed from `org-id-b36-to-int-one-digit'."
  (and (stringp i) (setq i (string-to-char i)))
  (cond ((and (>= i ?0) (<= i ?9)) (- i ?0))
        ((and (>= i ?A) (<= i ?Z)) (+ (- i ?A) 10))
        (t (error "Invalid b36 character"))))

(defun wg-int-to-b36 (i &optional length)
  "Return a base 36 string from I."
  (let ((base 36) b36)
    (cl-labels ((add-digit () (push (wg-int-to-b36-one-digit (mod i base)) b36)
                         (setq i (/ i base))))
      (add-digit)
      (while (> i 0) (add-digit))
      (setq b36 (cl-map 'string 'identity b36))
      (if (not length) b36
        (concat (make-string (max 0 (- length (length b36))) ?0) b36)))))

(defun wg-b36-to-int (str)
  "Convert STR, a base-36 string, into the corresponding integer.
Cribbed from `org-id-b36-to-int'."
  (let ((result 0))
    (mapc (lambda (i)
            (setq result (+ (* result 36)
                            (wg-b36-to-int-one-digit i))))
          str)
    result))



;;; lists

(defmacro wg-removef-p (item seq-place &rest keys)
  "If ITEM is a `member*' of SEQ-PLACE, remove it from SEQ-PLACE and return t.
Otherwise return nil.  KEYS can be any keywords accepted by `remove*'."
  `(> (length ,seq-place)
      (length (setf ,seq-place (cl-remove ,item ,seq-place ,@keys)))))

(defmacro wg-pushnew-p (item seq-place &rest keys)
  "If ITEM is not a `member' of SEQ-PLACE, push it to SEQ-PLACE and return t.
Otherwise return nil.  KEYS can be any keyword args accepted by `pushnew'."
  `(< (length ,seq-place)
      (length (cl-pushnew ,item ,seq-place ,@keys))))

(defun wg-range (start end)
  "Return a list of integers from START up to but not including END."
  (let (accum)
    (dotimes (i (- end start) (nreverse accum))
      (push (+ start i) accum))))

(defun wg-insert-after (elt list index)
  "Insert ELT into LIST after INDEX."
  (let ((new-list (cl-copy-list list)))
    (push elt (cdr (nthcdr index new-list)))
    new-list))

(defun wg-insert-before (elt list index)
  "Insert ELT into LIST before INDEX."
  (if (zerop index) (cons elt list)
    (wg-insert-after elt list (1- index))))

(defun wg-move-elt (elt list index &rest keys)
  "Move ELT before INDEX in LIST.
KEYS is passed to `remove*'."
  (wg-insert-before elt (apply 'cl-remove elt list keys) index))

(defun wg-cyclic-nth (list n)
  "Return the Nth element of LIST, modded by the length of list."
  (nth (mod n (length list)) list))

(defun wg-cyclic-offset-elt (elt list n)
  "Cyclically offset ELT's position in LIST by N."
  (-when-let (pos (cl-position elt list))
    (wg-move-elt elt list (mod (+ n pos) (length list)))))

(defun wg-cyclic-nth-from-elt (elt list n &rest keys)
  "Return the elt in LIST N places cyclically from ELT.
If ELT is not present is LIST, return nil.
KEYS is passed to `position'."
  (-when-let (pos (apply 'cl-position elt list keys))
    (wg-cyclic-nth list (+ pos n))))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 and ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (-when-let* ((p1 (cl-position elt1 list))
               (p2 (cl-position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

(defun wg-dups-p (list &rest keys)
  "Return non-nil when LIST contains duplicate elements.

Keywords supported: :test :key

\(fn LIST [KEYWORD VALUE]...)"
  (let ((test (or (plist-get keys :test) 'eq))
        (key (or (plist-get keys :key) 'identity)))
    (cl-loop for (elt . rest) on list
             for elt = (funcall key elt)
             when (cl-find elt rest :test test :key key) return elt)))

(defun wg-string-list-union (&optional list1 list2)
  "Return the `union' of LIST1 and LIST2, using `string=' as the test.
This only exists to get rid of duplicate lambdas in a few reductions."
  (cl-union list1 list2 :test 'string=))



;;; alists

(defun wg-make-alist (&rest kvps)
  "Return a new alist from KVPS."
  (let (alist)
    (while kvps
      (push (cons (car kvps) (cadr kvps)) alist)
      (setq kvps (cddr kvps)))
    (nreverse alist)))

(defun wg-aget (alist key &optional default)
  "Return the value of KEY in ALIST. Uses `assq'.
If PARAM is not found, return DEFAULT which defaults to nil."
  (aif (assq key alist) (cdr it) default))

(defun wg-acopy (alist)
  "Return a copy of ALIST's toplevel list structure."
  (mapcar (lambda (kvp) (cons (car kvp) (cdr kvp))) alist))

(defun wg-aput (alist key value)
  "Return a new alist from ALIST with KEY's value set to VALUE."
  (let* ((found nil)
         (new (wg-docar (kvp alist)
                (if (not (eq key (car kvp))) kvp
                  (setq found (cons key value))))))
    (if found new (cons (cons key value) new))))

(defun wg-aremove (alist key)
  "`remove' KEY's key-value-pair from ALIST."
  (remove (assoc key alist) alist))


;;; symbols and strings

(defun wg-toggle (symbol)
  "Toggle SYMBOL's truthiness."
  (set symbol (not (symbol-value symbol))))

(defun wg-symcat (&rest symbols-and-strings)
  "Return a new interned symbol by concatenating SYMBOLS-AND-STRINGS."
  (intern (mapconcat (lambda (obj) (if (symbolp obj) (symbol-name obj) obj))
                     symbols-and-strings "")))

(defun wg-make-string (times string &optional separator)
  "Like `make-string', but includes a separator."
  (mapconcat 'identity (make-list times string) (or separator "")))



;;; buffers

(defun wg-get-buffer (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer, or error."
  (or (get-buffer buffer-or-name)
      (error "%S does not identify a buffer" buffer-or-name)))

(defun wg-buffer-name (buffer-or-name)
  "Return BUFFER-OR-NAME's `buffer-name', or error."
  (buffer-name (wg-get-buffer buffer-or-name)))

(defun wg-buffer-major-mode (buffer-or-name)
  "Return BUFFER's major-mode."
  (with-current-buffer buffer-or-name major-mode))

(defun wg-current-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is the current buffer, nil otherwise."
  (eq (wg-get-buffer buffer-or-name) (current-buffer)))

;;; files

(defun wg-write-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (let ((print-level nil)  (print-length nil))
      (insert (format "%S" sexp)))
    (write-file file)))


;;; frames

(defun wg-cyclic-nth-from-frame (&optional n frame)
  "Return the frame N places away from FRAME in `frame-list' cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (wg-cyclic-nth-from-elt
   (or frame (selected-frame)) (frame-list) (or n 1)))



;;; namespace-prefixed defstruct

(defmacro wg-defstruct (prefix name-form &rest slot-defs)
  "`defstruct' wrapper that namespace-prefixes all generated functions.
Note: this doesn't yet work with :conc-name, and possibly other
options."
  (declare (indent 2))
  (let* ((name (if (consp name-form) (car name-form) name-form))
         (prefixed-name (wg-symcat prefix "-" name)))
    (cl-labels ((rebind (opstr)
                      (let ((oldfnsym (wg-symcat opstr "-" prefix "-" name)))
                        `((fset ',(wg-symcat prefix "-" opstr "-" name)
                                (symbol-function ',oldfnsym))
                          (fmakunbound ',oldfnsym)))))
      ;; `eval-and-compile' gets rid of byte-comp warnings ("function `foo' not
      ;; known to be defined").  We can accomplish this with `declare-function'
      ;; too, but it annoyingly requires inclusion of the function's arglist,
      ;; which gets ugly.
      `(eval-and-compile
         (cl-defstruct ,(if (symbolp name-form) prefixed-name
                       `(,prefixed-name ,@(cdr name-form)))
           ,@slot-defs)
         ,@(rebind "make")
         ,@(rebind "copy")
         ',prefixed-name))))

(defmacro wg-with-slots (obj slot-bindings &rest body)
  "Bind OBJ's slot values to symbols in BINDS, then eval BODY.
The car of each element of SLOT-BINDINGS is the bound symbol, and
the cadr as the accessor function."
  (declare (indent 2))
  (wg-with-gensyms (objsym)
    `(let* ((,objsym ,obj)
            ,@(wg-docar (slot slot-bindings)
                `(,(car slot) (,(cadr slot) ,objsym))))
       ,@body)))



;;; misc

(defun wg-add-or-remove-hooks (remove &rest pairs)
  "Add FUNCTION to or remove it from HOOK, depending on REMOVE."
  (dolist (pair (wg-partition pairs 2))
    (funcall (if remove 'remove-hook 'add-hook)
             (car pair) (cadr pair))))

(defmacro wg-set-parameter (place parameter value)
  "Set PARAMETER to VALUE at PLACE.
This needs to be a macro to allow specification of a setf'able place."
  (wg-with-gensyms (p v)
    `(let ((,p ,parameter) (,v ,value))
       (wg-pickelable-or-error ,p)
       (wg-pickelable-or-error ,v)
       (setf ,place (wg-aput ,place ,p ,v))
       ,v)))


;;; uid utils

(defun wg-time-to-b36 ()
  "Convert `current-time' into a b36 string."
  (apply 'concat (wg-docar (time (current-time))
                   (wg-int-to-b36 time 4))))

(defun wg-b36-to-time (b36)
  "Parse the time in B36 string from UID."
  (cl-loop for i from 0 to 8 by 4
           collect (wg-b36-to-int (cl-subseq b36 i (+ i 4)))))
(defalias 'wg-uid-to-time 'wg-b36-to-time)

(defun wg-generate-uid (&optional prefix)
  "Return a new uid, optionally prefixed by PREFIX."
  (concat prefix (wg-time-to-b36) "-" (wg-int-to-b36 string-chars-consed)))

(defun wg-uid-to-seconds (uid)
  "Return the `float-time' parsed from UID with `wg-uid-to-time'."
  (float-time (wg-uid-to-time uid)))


(defun wg-get-value (arg)
  "Get a value of ARG if it exists."
  (if (boundp `,arg) (eval arg)))

(defmacro wg-support (mode pkg params)
  "Macro to create (de)serialization functions for a buffer.
You need to save/restore a specific MODE which is loaded from a
package PKG.  In PARAMS you give local variables to save and a
deserialization function."
  `(let ((mode-str (symbol-name ,mode))
         (args ,params))

     (eval `(defun ,(intern (format "wg-deserialize-%s-buffer" mode-str)) (buffer)
              "DeSerialization function created with `wg-support'.
Gets saved variables and runs code to restore a BUFFER."
              (when (require ',,pkg nil 'noerror)
                (wg-dbind (this-function variables) (wg-buf-special-data buffer)
                  (let ((default-directory (car variables))
                        (df (cdr (assoc 'deserialize ',,params)))
                        (user-vars (car (cdr variables))))
                    (if df
                        (funcall df buffer user-vars)
                      (get-buffer-create wg-default-buffer))
                    )))))

     (eval `(defun ,(intern (format "wg-serialize-%s-buffer" mode-str)) (buffer)
              "Serialization function created with `wg-support'.
Saves some variables to restore a BUFFER later."
              (when (get-buffer buffer)
                (with-current-buffer buffer
                  (when (eq major-mode ',,mode)
                    (let ((sf (cdr (assoc 'serialize ',,params)))
                          (save (cdr (assoc 'save ',,params))))
                      (list ',(intern (format "wg-deserialize-%s-buffer" mode-str))
                            (list default-directory
                                  (if sf (funcall sf buffer)
                                    (if save (mapcar 'wg-get-value save)))
                                  ))))))))
     ;; Maybe change a docstring for functions
     ;;(put (intern (format "wg-serialize-%s-buffer" (symbol-name mode)))
     ;;     'function-documentation
     ;;     (format "A function created by `wg-support'."))

     ;; Add function to `wg-special-buffer-serdes-functions' variable
     (eval `(add-to-list 'wg-special-buffer-serdes-functions
                         ',(intern (format "wg-serialize-%s-buffer" mode-str)) t))
     ))

(defvar wg-current-session nil "Current session object.")
(defun wg-current-session (&optional noerror)
  "Return `wg-current-session' or scream unless NOERROR."
  (or wg-current-session
      (unless noerror
        (error "No session is defined"))))

;; locate-dominating-file
(defun wg-get-first-existing-dir (&optional dir)
  "Test if DIR exists and return it.
If not - try to go to the parent dir and do the same."
  (let* ((d (or dir default-directory)))
    (if (file-directory-p d) d
      (let* ((cur d) (parent (file-name-directory (directory-file-name cur))))
        (while (and (> (length cur) (length parent))
                    (not (file-directory-p parent)))
          (message "Test %s" parent)
          (setq cur parent)
          (setq parent (file-name-directory (directory-file-name cur))))
        parent))))

(provide 'workgroups-utils-basic)
;;; workgroups-utils-basic.el ends here
