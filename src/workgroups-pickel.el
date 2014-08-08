;;; workgroups-pickel.el --- Elisp object serdes used by Workgroups
;;; Commentary:
;;
;; Workgroups allows you to serialize some objects, even such as buffers
;; (that are displayed as #<object>). The only trick is to write
;; functions that extract enough information about an object and
;; functions that can recreate an object.
;;
;; Main functions are: `wg-pickel', `wg-pickel-to-string', `wg-pickel-to-file'
;; What objects are supported? See `wg-pickel-pickelable-types'
;;
;;; Code:

(require 'cl-lib)
(require 'workgroups-utils-basic)

(defvar wg-pickel-identifier '~pickel!~
  "Symbol identifying a stream as a pickel.")

(defvar wg-pickel-pickelable-types
  '(integer
    float
    symbol
    string
    cons
    vector
    hash-table
    buffer
    marker
    ;;window-configuration
    ;;frame
    ;;window
    ;;process
    )
  "Types pickel can serialize.")

(defvar wg-pickel-object-serializers
  '((integer    . identity)
    (float      . identity)
    (string     . identity)
    (symbol     . wg-pickel-symbol-serializer)
    (cons       . wg-pickel-cons-serializer)
    (vector     . wg-pickel-vector-serializer)
    (hash-table . wg-pickel-hash-table-serializer)
    (buffer     . wg-pickel-buffer-serializer)
    (marker     . wg-pickel-marker-serializer)
    ;;(window-configuration   . wg-pickel-window-configuration-serializer)
    )
  "Alist mapping types to object serialization functions.")
(defvar wg-pickel-object-deserializers
  '((s . wg-pickel-deserialize-uninterned-symbol)
    (c . wg-pickel-deserialize-cons)
    (v . wg-pickel-deserialize-vector)
    (h . wg-pickel-deserialize-hash-table)
    (b . wg-pickel-deserialize-buffer)
    (m . wg-pickel-deserialize-marker)
    ;;(f . wg-pickel-deserialize-frame)
    )
  "Alist mapping type keys to object deserialization functions.")

(defvar wg-pickel-link-serializers
  '((cons       . wg-pickel-cons-link-serializer)
    (vector     . wg-pickel-vector-link-serializer)
    (hash-table . wg-pickel-hash-table-link-serializer))
  "Alist mapping types to link serialization functions.")
(defvar wg-pickel-link-deserializers
  `((c . wg-pickel-cons-link-deserializer)
    (v . wg-pickel-vector-link-deserializer)
    (h . wg-pickel-hash-table-link-deserializer))
  "Alist mapping type keys to link deserialization functions.")



;;; errors and predicates

(put 'wg-pickel-unpickelable-type-error
     'error-conditions
     '(error wg-pickel-errors wg-pickel-unpickelable-type-error))

(put 'wg-pickel-unpickelable-type-error
     'error-message
     "Attemp to pickel unpickelable type")

(defun wg-pickelable-or-error (obj)
  "Error when OBJ isn't pickelable."
  (unless (memq (type-of obj) wg-pickel-pickelable-types)
    (signal 'wg-pickel-unpickelable-type-error
            (format "Can't pickel objects of type: %S" (type-of obj))))
  (cl-typecase obj
    (cons
     (wg-pickelable-or-error (car obj))
     (wg-pickelable-or-error (cdr obj)))
    (vector
     (cl-map nil 'wg-pickelable-or-error obj))
    (hash-table
     (wg-dohash (key value obj)
       (wg-pickelable-or-error key)
       (wg-pickelable-or-error value)))))

(defun wg-pickelable-p (obj)
  (condition-case err
      (progn (wg-pickelable-or-error obj) t)
    (wg-pickel-unpickelable-type-error nil)))

(defun wg-pickel-p (obj)
  "Return t when OBJ is a pickel, nil otherwise."
  (and (consp obj) (eq (car obj) wg-pickel-identifier)))



;; accessor functions

(defun wg-pickel-object-serializer (obj)
  "Return the object serializer for the `type-of' OBJ."
  (or (wg-aget wg-pickel-object-serializers (type-of obj))
      (error "Invalid type: %S" (type-of obj))))

(defun wg-pickel-link-serializer (obj)
  "Return the link serializer for the `type-of' OBJ."
  (wg-aget wg-pickel-link-serializers (type-of obj)))

(defun wg-pickel-object-deserializer (key)
  "Return the object deserializer for type key KEY, or error."
  (or (wg-aget wg-pickel-object-deserializers key)
      (error "Invalid object deserializer key: %S" key)))

(defun wg-pickel-link-deserializer (key)
  "Return the link deserializer for type key KEY, or error."
  (or (wg-aget wg-pickel-link-deserializers key)
      (error "Invalid link deserializer key: %S" key)))



;;; bindings

(defun wg-pickel-make-bindings-table (obj)
  "Return a table binding unique subobjects of OBJ to ids."
  (let ((binds (make-hash-table :test 'eq))
        (id -1))
    (cl-labels
     ((inner (obj)
           (unless (gethash obj binds)
              (puthash obj (cl-incf id) binds)
              (cl-case (type-of obj)
                (cons
                 (inner (car obj))
                 (inner (cdr obj)))
                (vector
                 (dotimes (idx (length obj))
                   (inner (aref obj idx))))
                (hash-table
                 (wg-dohash (key val obj)
                   (inner key)
                   (inner val)))))))
      (inner obj)
      binds)))



;;; Objects

;; symbol
;; (wg-unpickel (wg-pickel 123))
;; (wg-unpickel (wg-pickel "table"))
;; (wg-unpickel (wg-pickel 'test))
(defun wg-pickel-symbol-serializer (symbol)
  "Return SYMBOL's serialization."
  (cond ((eq symbol t) t)
        ((eq symbol nil) nil)
        ((intern-soft symbol) symbol)
        (t (list 's (symbol-name symbol)))))
(defun wg-pickel-deserialize-uninterned-symbol (name)
  "Return a new uninterned symbol from NAME."
  (make-symbol name))


;; buffer
;; (wg-unpickel (wg-pickel (current-buffer)))
;; (wg-unpickel (wg-pickel (get-buffer org-agenda-buffer-name)))
(defun wg-pickel-buffer-serializer (buffer)
  "Return BUFFER's UID in workgroups buffer list."
  (list 'b (wg-add-buffer-to-buf-list buffer)))
(defun wg-pickel-deserialize-buffer (uid)
  "Return a restored buffer from it's UID."
  (wg-restore-buffer (wg-find-buf-by-uid uid)))


;; marker
;; (wg-unpickel (wg-pickel (point-marker)))
(defun wg-pickel-marker-serializer (marker)
  "Return MARKERS's data."
  (list 'm (list (marker-position marker)
                 (wg-add-buffer-to-buf-list (marker-buffer marker)))))
(defun wg-pickel-deserialize-marker (data)
  "Return marker from it's data."
  (let ((m (make-marker)))
    (set-marker m (car data) (wg-pickel-deserialize-buffer (car (cdr data))))))


;; cons - http://www.gnu.org/software/emacs/manual/html_node/eintr/cons.html
(defun wg-pickel-cons-serializer (cons)
  "Return CONS's serialization."
  (list 'c))
(defun wg-pickel-deserialize-cons ()
  "Return a new cons cell initialized to nil."
  (cons nil nil))
(defun wg-pickel-cons-link-serializer (cons binds)
  "Return the serialization of CONS's links in BINDS."
  (list 'c
        (gethash cons binds)
        (gethash (car cons) binds)
        (gethash (cdr cons) binds)))
(defun wg-pickel-cons-link-deserializer (cons-id car-id cdr-id binds)
  "Relink a cons cell with its car and cdr in BINDS."
  (let ((cons (gethash cons-id binds)))
    (setcar cons (gethash car-id binds))
    (setcdr cons (gethash cdr-id binds))))



;; vector - http://www.gnu.org/software/emacs/manual/html_node/elisp/Vector-Functions.html
;; (wg-unpickel (wg-pickel (make-vector 9 'Z)))
;;
(defun wg-pickel-vector-serializer (vector)
  "Return VECTOR's serialization."
  (list 'v (length vector)))
(defun wg-pickel-deserialize-vector (length)
  "Return a new vector of length LENGTH."
  (make-vector length nil))
(defun wg-pickel-vector-link-serializer (vector binds)
  "Return the serialization of VECTOR's links in BINDS."
  (let (result)
    (dotimes (i (length vector) result)
      (setq result
            (nconc (list 'v
                         (gethash vector binds)
                         i
                         (gethash (aref vector i) binds))
                   result)))))
(defun wg-pickel-vector-link-deserializer (vector-id index value-id binds)
  "Relink a vector with its elements in BINDS."
  (aset (gethash vector-id binds) index (gethash value-id binds)))


;; hash table - http://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html
(defun wg-pickel-hash-table-serializer (table)
  "Return HASH-TABLE's serialization."
  (list 'h
        (hash-table-test table)
        (hash-table-size table)
        (hash-table-rehash-size table)
        (hash-table-rehash-threshold table)
        (hash-table-weakness table)))
(defun wg-pickel-deserialize-hash-table (test size rsize rthresh weakness)
  "Return a new hash-table with the specified properties."
  (make-hash-table :test test :size size :rehash-size rsize
                   :rehash-threshold rthresh :weakness weakness))
(defun wg-pickel-hash-table-link-serializer (table binds)
  "Return the serialization of TABLE's links in BINDS."
  (let (result)
    (wg-dohash (key value table result)
      (setq result
            (nconc (list 'h
                         (gethash key binds)
                         (gethash value binds)
                         (gethash table binds))
                   result)))))
(defun wg-pickel-hash-table-link-deserializer (key-id value-id table-id binds)
  "Relink a hash-table with its keys and values in BINDS."
  (puthash (gethash key-id binds)
           (gethash value-id binds)
           (gethash table-id binds)))



;; TODO
(defun wg-pickel-window-configuration-serializer (wc)
  "Return Window configuration WC's serialization."
  (list 'wc 1))





(defun wg-pickel-serialize-objects (binds)
  "Return a list of serializations of the objects in BINDS."
  (let (result)
    (wg-dohash (obj id binds result)
      (setq result
            (nconc (list id (funcall (wg-pickel-object-serializer obj) obj))
                   result)))))
(defun wg-pickel-deserialize-objects (serial-objects)
  "Return a hash-table of objects deserialized from SERIAL-OBJECTS."
  (let ((binds (make-hash-table)))
    (wg-destructuring-dolist ((id obj . rest) serial-objects binds)
      (puthash id
               (if (atom obj) obj
                 (wg-dbind (key . data) obj
                   (apply (wg-pickel-object-deserializer key) data)))
               binds))))



(defun wg-pickel-serialize-links (binds)
  "Return a list of serializations of the links between objects in BINDS."
  (let (result)
    (wg-dohash (obj id binds result)
      (wg-awhen (wg-pickel-link-serializer obj)
        (setq result (nconc (funcall it obj binds) result))))))
(defun wg-pickel-deserialize-links (serial-links binds)
  "Return BINDS after relinking all its objects according to SERIAL-LINKS."
  (wg-destructuring-dolist ((key arg1 arg2 arg3 . rest) serial-links binds)
    (funcall (wg-pickel-link-deserializer key) arg1 arg2 arg3 binds)))



;;; pickeling

(defun wg-pickel (obj)
  "Return the serialization of OBJ."
  (wg-pickelable-or-error obj)
  (let ((binds (wg-pickel-make-bindings-table obj)))
    (list wg-pickel-identifier
          (wg-pickel-serialize-objects binds)
          (wg-pickel-serialize-links binds)
          (gethash obj binds))))

(defun wg-pickel-to-string (obj)
  "Serialize OBJ to a string and return the string."
  (format "%S" (wg-pickel obj)))

(defun wg-pickel-to-file (file obj)
  "Serialize OBJ to FILE."
  (wg-write-sexp-to-file (wg-pickel obj) file))



;;; unpickeling

(defun wg-unpickel (pickel)
  "Return the deserialization of PICKEL."
  (unless (wg-pickel-p pickel)
    (error "Attempt to unpickel a non-pickel."))
  (wg-dbind (id serial-objects serial-links result) pickel
    (gethash
     result
     (wg-pickel-deserialize-links
      serial-links
      (wg-pickel-deserialize-objects serial-objects)))))

(defun wg-unpickel-file (file)
  "`unpickel' an object directly from FILE."
  (wg-unpickel (wg-lisp-object-from-file file)))

(defun wg-unpickel-string (str)
  "`unpickel' and object directly from STR."
  (wg-unpickel (read str)))

(provide 'workgroups-pickel)
;;; workgroups-pickel.el ends here
