;;; workgroups-buf.el --- BUFFER class
;;; Commentary:
;;
;; Workgroups Data Structures:
;;   https://github.com/pashinin/workgroups2/wiki/Workgroups-data-structures
;;
;;
;; BUFFER is the most low level part Workgroups operates with (except
;; serializing Emacs objects functions).
;;
;; Different buffers we have:
;;  - live buffers     (just switch-to them)
;;  - files/dirs       (open them)
;;  - special buffers  (shells, unknown modes - write support for them)
;;
;; Another different types of "buffers":
;;  - standard Emacs buffer (as you know it)
;;  - Workgroups Buffer object (Elisp object, a representation of Emacs buffer)
;;; Code:

(require 'workgroups-pickel)
(require 'workgroups-specialbufs)
(require 'workgroups-structs)

;;; Variables

(defvar wg-buffer-workgroup nil
  "A workgroup in which this buffer most recently appeared.
Buffer-local.")
(make-variable-buffer-local 'wg-buffer-workgroup)

(defcustom wg-default-buffer "*scratch*"
  "Show this in case everything else fails.
When a buffer can't be restored, when creating a blank wg."
  :type 'string
  :group 'workgroups)


;;; Functions

(defmacro wg-buf-list ()
  "Setf'able `wg-current-session' buf-list slot accessor."
  `(wg-session-buf-list (wg-current-session)))

(defun wg-restore-default-buffer (&optional switch)
  "Return `wg-default-buffer' and maybe SWITCH to it."
  (if switch
      (switch-to-buffer wg-default-buffer t)
    (get-buffer-create wg-default-buffer)))

(defun wg-restore-existing-buffer (buf &optional switch)
  "Return existing buffer from BUF and maybe SWITCH to it."
  (-when-let (b (wg-find-buf-in-buffer-list buf (wg-buffer-list-emacs)))
    (if switch (switch-to-buffer b t))
    (with-current-buffer b
      (wg-set-buffer-uid-or-error (wg-buf-uid buf))
      b)))

(defun wg-restore-file-buffer (buf &optional switch)
  "Restore BUF by finding its file and maybe SWITCH to it.
Return the created buffer.
If BUF's file doesn't exist, call `wg-restore-default-buffer'"
  ;;(-when-let ((file-name (wg-buf-file-name buf)))
  (let ((file-name (wg-buf-file-name buf)))
    (when (and file-name
               (or wg-restore-remote-buffers
                   (not (file-remote-p file-name))))
      (cond ((file-exists-p file-name)
             ;; jit ignore errors
             ;;(ignore-errors
             (condition-case err
                 (let ((b (find-file-noselect file-name nil nil nil)))
                   (with-current-buffer b
                     (rename-buffer (wg-buf-name buf) t)
                     (wg-set-buffer-uid-or-error (wg-buf-uid buf))
                     (when wg-restore-mark
                       (set-mark (wg-buf-mark buf))
                       (deactivate-mark))
                     (wg-deserialize-buffer-local-variables buf)
                     )
                   (if switch (switch-to-buffer b))
                   b)
               (error
                (message "Error while restoring a file %s:\n  %s" file-name (error-message-string err))
                nil)))
            (t
             ;; try directory
             (if (file-directory-p (file-name-directory file-name))
                 (dired (file-name-directory file-name))
               (progn
                 (message "Attempt to restore nonexistent file: %S" file-name)
                 nil))
             )))))

(defun wg-restore-special-buffer (buf &optional switch)
  "Restore a buffer BUF with DESERIALIZER-FN and maybe SWITCH to it."
  (-when-let*
      ((special-data (wg-buf-special-data buf))
       (buffer (save-window-excursion
                 (condition-case err
                     (funcall (car special-data) buf)
                   (error (message "Error deserializing %S: %S" (wg-buf-name buf) err)
                          nil)))))
    (if switch (switch-to-buffer buffer t))
    (with-current-buffer buffer
      (wg-set-buffer-uid-or-error (wg-buf-uid buf)))
    buffer))

(defun wg-restore-buffer (buf &optional switch)
  "Restore BUF, return it and maybe SWITCH to it."
  (fset 'buffer-list wg-buffer-list-original)
  (prog1
      (or (wg-restore-existing-buffer buf switch)
          (wg-restore-special-buffer buf switch)  ;; non existent dired problem
          (wg-restore-file-buffer buf switch)
          (progn (wg-restore-default-buffer switch) nil))
    (if wg-mess-with-buffer-list
        (fset 'buffer-list wg-buffer-list-function))))



;;; buffer object utils

(defun wg-buffer-uid (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer-local value of `wg-buffer-uid'."
  (buffer-local-value 'wg-buffer-uid (wg-get-buffer buffer-or-name)))

(defun wg-bufobj-uid (bufobj)
  "Return BUFOBJ's uid."
  (cl-etypecase bufobj
    (buffer (wg-buffer-uid bufobj))
    (wg-buf (wg-buf-uid bufobj))
    (string (wg-bufobj-uid (wg-get-buffer bufobj)))))

(defun wg-bufobj-name (bufobj)
  "Return BUFOBJ's buffer name."
  (cl-etypecase bufobj
    (buffer (buffer-name bufobj))
    (wg-buf (wg-buf-name bufobj))
    (string (wg-buffer-name bufobj))))

(defun wg-bufobj-file-name (bufobj)
  "Return BUFOBJ's filename."
  (cl-etypecase bufobj
    (buffer (buffer-file-name bufobj))
    (wg-buf (wg-buf-file-name bufobj))
    (string (wg-bufobj-file-name (wg-get-buffer bufobj)))))

(defun wg-buf-major-mode (buf)
  "Return BUF's `major-mode'.
It's stored in BUF's local-vars list, since it's a local variable."
  (wg-aget (wg-buf-local-vars buf) 'major-mode))

(defun wg-buffer-major-mode (bufobj)
  "Return BUFOBJ's `major-mode'.
It works with Emacs buffer, Workgroups buffer object and a simple string."
  (cl-etypecase bufobj
    (buffer (wg-buffer-major-mode bufobj))
    (wg-buf (wg-buf-major-mode bufobj))
    (string (wg-buffer-major-mode bufobj))))

;; `wg-equal-bufobjs' and `wg-find-bufobj' may need to be made a lot smarter
(defun wg-equal-bufobjs (bufobj1 bufobj2)
  "Return t if BUFOBJ1 is \"equal\" to BUFOBJ2."
  (let ((fname1 (wg-bufobj-file-name bufobj1))
        (fname2 (wg-bufobj-file-name bufobj2)))
    (cond ((and fname1 fname2) (string= fname1 fname2))
          ((or fname1 fname2) nil)
          ((string= (wg-bufobj-name bufobj1) (wg-bufobj-name bufobj2)) t))))

(defun wg-find-bufobj (bufobj bufobj-list)
  "Find BUFOBJ in BUFOBJ-LIST, testing with `wg-equal-bufobjs'."
  (cl-find bufobj bufobj-list :test 'wg-equal-bufobjs))

(defun wg-find-bufobj-by-uid (uid bufobj-list)
  "Find the bufobj in BUFOBJ-LIST with uid UID."
  (cl-find uid bufobj-list :test 'string= :key 'wg-bufobj-uid))

(defun wg-find-buf-in-buf-list (buf buf-list)
  "Find BUF in BUF-LIST.
This is only here for completeness."
  (cl-find buf buf-list))

(defun wg-find-buffer-in-buffer-list (buffer-or-name buffer-list)
  "Find BUFFER-OR-NAME in BUFFER-LIST."
  (cl-find (wg-get-buffer buffer-or-name) buffer-list :key 'wg-get-buffer))

(defun wg-find-buffer-in-buf-list (buffer-or-name buf-list)
  "Find BUFFER-OR-NAME in BUF-LIST."
  (aif (wg-buffer-uid buffer-or-name)
      (wg-find-bufobj-by-uid it buf-list)
    (wg-find-bufobj buffer-or-name buf-list)))

(defun wg-find-buf-in-buffer-list (buf buffer-list)
  "Find BUF in BUFFER-LIST."
  (or (wg-find-bufobj-by-uid (wg-buf-uid buf) buffer-list)
      (wg-find-bufobj buf buffer-list)))

(defun wg-find-buf-by-uid (uid)
  "Find a buf in `wg-buf-list' by UID."
  (wg-find-bufobj-by-uid uid (wg-buf-list)))

(defun wg-set-buffer-uid-or-error (uid &optional buffer)
  "Set BUFFER's buffer local value of `wg-buffer-uid' to UID.
If BUFFER already has a buffer local value of `wg-buffer-uid',
and it's not equal to UID, error."
  (if wg-buffer-uid
      ;;(if (string= wg-buffer-uid uid) uid
      ;;  (error "uids don't match %S and %S" uid wg-buffer-uid))
      (setq wg-buffer-uid uid)))


(defun wg-buffer-special-data (buffer)
  "Return BUFFER's auxiliary serialization, or nil."
  (cl-some (lambda (fn) (funcall fn buffer)) wg-special-buffer-serdes-functions))


(defun wg-serialize-buffer-local-variables ()
  "Return an alist of buffer-local variable symbols and their values.
See `wg-buffer-local-variables-alist' for details."
  (wg-docar (entry wg-buffer-local-variables-alist)
    (wg-dbind (var ser des) entry
      (when (local-variable-p var)
        (cons var (if ser (funcall ser) (symbol-value var)))))))

(defun wg-buffer-to-buf (buffer)
  "Return the serialization (a wg-buf) of Emacs buffer BUFFER."
  (with-current-buffer buffer
    (wg-make-buf
     :name           (buffer-name)
     :file-name      (buffer-file-name)
     :point          (point)
     :mark           (mark)
     :local-vars     (wg-serialize-buffer-local-variables)
     :special-data   (wg-buffer-special-data buffer))))

(defun wg-add-buffer-to-buf-list (buffer)
  "Make a buf from BUFFER, and add it to `wg-buf-list' if necessary.
If there isn't already a buf corresponding to BUFFER in
`wg-buf-list', make one and add it.  Return BUFFER's uid
in either case."
  (with-current-buffer buffer
    (setq wg-buffer-uid
          (aif (wg-find-buffer-in-buf-list buffer (wg-buf-list))
              (wg-buf-uid it)
            (let ((buf (wg-buffer-to-buf buffer)))
              (push buf (wg-buf-list))
              (wg-buf-uid buf))))))

(defun wg-buffer-uid-or-add (buffer)
  "Return BUFFER's uid.
If there isn't already a buf corresponding to BUFFER in
`wg-buf-list', make one and add it."
  (or (wg-buffer-uid buffer) (wg-add-buffer-to-buf-list buffer)))

(defun wg-bufobj-uid-or-add (bufobj)
  "If BUFOBJ is a wg-buf, return its uid.
If BUFOBJ is a buffer or a buffer name, see `wg-buffer-uid-or-add'."
  (cl-etypecase bufobj
    (wg-buf (wg-buf-uid bufobj)) ;; possibly also add to `wg-buf-list'
    (buffer (wg-buffer-uid-or-add bufobj))
    (string (wg-bufobj-uid-or-add (wg-get-buffer bufobj)))))


(defun wg-reset-buffer (buffer)
  "Return BUFFER.
Currently only sets BUFFER's `wg-buffer-uid' to nil."
  (with-current-buffer buffer (setq wg-buffer-uid nil)))



;;; buffer-list-filter commands

(defun wg-update-buffer-in-buf-list (&optional buffer)
  "Update BUFFER's corresponding buf in `wg-buf-list'.
BUFFER nil defaults to `current-buffer'."
  (let ((buffer (or buffer (current-buffer))))
    (-when-let* ((uid (wg-buffer-uid buffer))
                 (old-buf (wg-find-buf-by-uid uid))
                 (new-buf (wg-buffer-to-buf buffer)))
      (setf (wg-buf-uid new-buf) (wg-buf-uid old-buf))
      (wg-asetf (wg-buf-list) (cons new-buf (remove old-buf it))))))

(defun wg-update-buf-list (&optional buffer-list)
  "Update all bufs in `wg-buf-list' corresponding to buffers in BUFFER-LIST."
  (mapc 'wg-update-buffer-in-buf-list (or buffer-list (wg-buffer-list-emacs))))

(defun wg-buffer-list-display (buffer-list)
  "Return the BUFFER-LIST display string."
  (wg-display-internal 'wg-buffer-display buffer-list))

;; buffer-list filters

(defun wg-buffer-list-filter-all (workgroup initial)
  "Return all buffers in INITIAL."
  initial)

;; buffer-list-filter context

(defmacro wg-prior-mapping (mode command)
  "Return whatever COMMAND would call if MODE wasn't on."
  `(or (let (,mode) (command-remapping ,command)) ,command))

(provide 'workgroups-buf)
;;; workgroups-buf.el ends here
