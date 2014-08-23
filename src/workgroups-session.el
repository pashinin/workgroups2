;;; workgroups-session.el --- Top level structure "session"
;;; Commentary:
;; Main function are: `wg-write-session-file'
;;; Code:

(require 'workgroups-variables)
(require 'workgroups-workgroup)



;;
;; Variables
;;

(defcustom wg-session-file "~/.emacs_workgroups"
  "Default filename to be used to save workgroups."
  :type 'file
  :group 'workgroups)
(defvaralias 'wg-default-session-file 'wg-session-file)

(defvar wg-incorrectly-restored-bufs nil
  "FIXME: docstring this.")
;; TODO: check it on switching WG

(defvar wg-record-incorrectly-restored-bufs nil
  "FIXME: docstring this.")

(defcustom wg-emacs-exit-save-behavior 'save
  "Determines save behavior on Emacs exit.
Possible values:

`ask'           Ask the user whether to save if there are unsaved changes

`save'          Call `wg-save-session' when there are unsaved changes

Anything else   Exit Emacs without saving changes"
  :type 'symbol
  :group 'workgroups)

(defcustom wg-workgroups-mode-exit-save-behavior 'save
  "Determines save behavior on `workgroups-mode' exit.
Possible values:

`ask'           Ask the user whether to saveif there are unsaved changes

`save'          Call `wg-save-session' when there are unsaved changes

Anything else   Exit `workgroups-mode' without saving changes"
  :type 'symbol
  :group 'workgroups)



;;
;; Function
;;

(defun wg-session-uids-consistent-p ()
  "Return t if there are no duplicate bufs or buf uids in the wrong places.
nil otherwise."
  (and (not (wg-dups-p (wg-buf-list) :key 'wg-buf-uid :test 'string=))
       (not (wg-dups-p (wg-workgroup-list) :key 'wg-workgroup-uid :test 'string=))))


(defun wg-find-session-file (filename)
  "Load a session visiting FILENAME, creating one if none already exists."
  (interactive "FFind session file: ")
  (cond ((file-exists-p filename)
         (let ((session (wg-read-sexp-from-file filename)))
           (unless (wg-session-p session)
             (error "%S is not a Workgroups session file." filename))
           (setf (wg-session-file-name session) filename)
           (wg-reset-internal (wg-unpickel-session-parameters session)))

         (if wg-control-frames
             (wg-restore-frames))

         (wg-awhen (wg-workgroup-list)
           (if (and wg-open-this-wg
                    (member wg-open-this-wg (wg-workgroup-names)))
               (wg-switch-to-workgroup wg-open-this-wg)
             (if (and wg-load-last-workgroup
                      (member (wg-session-parameter (wg-current-session t) 'last-workgroup)
                              (wg-workgroup-names)))
                 (wg-switch-to-workgroup
                  (wg-session-parameter (wg-current-session t) 'last-workgroup))
               (wg-switch-to-workgroup (car it)))
             ))
         (wg-fontified-message (:cmd "Loaded: ") (:file filename)))
        (t
         (wg-query-and-save-if-modified)
         (wg-reset-internal (wg-make-session :file-name filename))
         (wg-fontified-message
           (:cmd "(New Workgroups session file)")))))
(defalias 'wg-open-session 'wg-find-session-file)

(defun wg-write-session-file (filename &optional confirm)
  "Write the current session into file FILENAME.
This makes the session visit that file, and marks it as not modified.

If optional second arg CONFIRM is non-nil, this function asks for
confirmation before overwriting an existing file.  Interactively,
confirmation is required unless you supply a prefix argument.

Think of it as `write-file' for Workgroups sessions."
  (interactive (list (read-file-name "Save session as: ")
                     (not current-prefix-arg)))
  (when (and confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File `%s' exists; overwrite? " filename))
      (error "Cancelled")))
  (unless (file-writable-p filename)
    (error "File %s can't be written to" filename))
  (wg-perform-session-maintenance)
  (setf (wg-session-file-name (wg-current-session)) filename)
  (setf (wg-session-version (wg-current-session)) wg-version)
  (if wg-control-frames
      (wg-save-frames))
  (wg-write-sexp-to-file
   (wg-pickel-all-session-parameters (wg-current-session))
   filename)
  (wg-mark-everything-unmodified)
  (wg-fontified-message (:cmd "Wrote: ") (:file filename)))
(defalias 'wg-save-session-as 'wg-write-session-file)

(defun wg-determine-session-save-file-name ()
  "Return the filename in which to save the session."
  (or (wg-session-file-name (wg-current-session))
      (and wg-session-load-on-start wg-session-file)))

(defun wg-save-session (&optional force)
  "Save the current Workgroups session if it's been modified.
Think of it as `save-buffer' for Workgroups sessions.  Optional
argument FORCE non-nil, or interactively with a prefix arg, save
the session regardless of whether it's been modified."
  (interactive "P")
  (if (and (not (wg-modified-p)) (not force))
      (wg-message "(The session is unmodified)")
    (wg-write-session-file
     (or (wg-determine-session-save-file-name)
         (read-file-name "Save session as: ")))))

(defun wg-reset-internal (&optional session)
  "Reset Workgroups, setting `wg-current-session' to SESSION.
Resets all frame parameters, buffer-local vars, current
Workgroups session object, etc.  SESSION nil defaults to a new,
blank session object."
  (mapc 'wg-reset-frame (frame-list))
  (mapc 'wg-reset-buffer (wg-buffer-list-emacs))
  (setq wg-wconfig-kill-ring nil)
  (setq wg-current-session (or session (wg-make-session))))



(defun wg-session-all-buf-uids (&optional session)
  "Return a new list of all unique buf uids in SESSION.
SESSION nil defaults to `wg-current-session'."
  (cl-reduce 'wg-string-list-union
             (wg-session-workgroup-list (or session (wg-current-session)))
             :key 'wg-workgroup-all-buf-uids))

(defun wg-buffer-list-all-uids (&optional buffer-list)
  "Return a list of the uids of all buffers in BUFFER-LIST in
which `wg-buffer-uid' is locally bound.
BUFFER-LIST nil defaults to `buffer-list'."
  (delq nil (mapcar 'wg-buffer-uid (or buffer-list (wg-buffer-list-emacs)))))

(defun wg-all-buf-uids (&optional session buffer-list)
  "Return the union of `wg-session-all-buf-uids' and `wg-buffer-list-all-uids'."
  (cl-union (wg-session-all-buf-uids session)
            (wg-buffer-list-all-uids buffer-list)
            :test 'string=))

(defun wg-gc-bufs ()
  "gc bufs from `wg-buf-list' that are no longer needed."
  (let ((all-buf-uids (wg-all-buf-uids)))
    (wg-asetf (wg-buf-list)
              (cl-remove-if-not (lambda (uid) (member uid all-buf-uids)) it
                                :key 'wg-buf-uid))))



;; FIXME: Duplicate buf names probably shouldn't be allowed.  An unrelated error
;; causes two *scratch* buffers to be present, triggering the "uids don't match"
;; error.  Write something to remove bufs with duplicate names.


(defun wg-perform-session-maintenance ()
  "Perform various maintenance operations on the current Workgroups session."
  (wg-update-current-workgroup-working-wconfig)

  ;; Update every workgroup's base wconfig with `wg-workgroup-update-base-wconfig'
  (dolist (workgroup (wg-workgroup-list))
    (wg-awhen (wg-workgroup-selected-frame-wconfig workgroup)
      (setf (wg-workgroup-base-wconfig workgroup) it
            (wg-workgroup-selected-frame-wconfig workgroup) nil)))

  (wg-gc-bufs)
  (wg-gc-buf-uids)
  (wg-update-buf-list))


;; session consistency testing


(defun wg-modified-p ()
  "Return t when the current session or any of its workgroups are modified."
  (or (wg-session-modified (wg-current-session))
      (cl-some 'wg-workgroup-modified (wg-workgroup-list))))

(defun wg-mark-everything-unmodified ()
  "Mark the session and all workgroups as unmodified."
  (setf (wg-session-modified (wg-current-session)) nil)
  (dolist (workgroup (wg-workgroup-list))
    (setf (wg-workgroup-modified workgroup) nil)))


(defun wg-workgroup-names (&optional noerror)
  "Return a list of workgroup names or scream unless NOERROR."
  (mapcar 'wg-workgroup-name (wg-workgroup-list-or-error noerror)))


;;; session parameters

(defun wg-session-parameter (session parameter &optional default)
  "Return SESSION's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
SESSION nil defaults to the current session."
  (wg-aget (wg-session-parameters (or session (wg-current-session)))
           parameter default))

(defun wg-set-session-parameter (session parameter value)
  "Set SESSION's value of PARAMETER to VALUE.
SESSION nil means use the current session.
Return value."
  (let ((session (or session (wg-current-session))))
    (wg-set-parameter (wg-session-parameters session) parameter value)
    (setf (wg-session-modified session) t)
    value))

(defun wg-remove-session-parameter (session parameter)
  "Remove parameter PARAMETER from SESSION's parameters."
  (let ((session (or session (wg-current-session))))
    (wg-asetf (wg-session-parameters session) (wg-aremove it parameter))
    (setf (wg-session-modified session) t)))

(defun wg-session-local-value (variable &optional session)
  "Return the value of VARIABLE in SESSION.
SESSION nil defaults to the current session.  If VARIABLE does
not have a session-local binding in SESSION, the value is
resolved by Emacs."
  (let* ((undefined (cl-gensym))
         (value (wg-session-parameter session variable undefined)))
    (if (not (eq value undefined)) value
      (symbol-value variable))))

(defun wg-reset-frame (frame)
  "Reset Workgroups' `frame-parameters' in FRAME to nil."
  (set-frame-parameter frame 'wg-workgroup-state-table nil)
  (set-frame-parameter frame 'wg-current-workgroup-uid nil)
  (set-frame-parameter frame 'wg-previous-workgroup-uid nil))

(defun wg-save-session-on-exit (behavior)
  "Perform session-saving operations based on BEHAVIOR."
  (cl-case behavior
    (ask (wg-query-and-save-if-modified))
    (save
     (if (wg-determine-session-save-file-name)
         (wg-save-session)
       (wg-query-and-save-if-modified)))))

(defun wg-save-frames ()
  "Save opened frames as a session parameter.
Exclude `selected-frame' and daemon one (if any).
http://stackoverflow.com/questions/21151992/why-emacs-as-daemon-gives-1-more-frame-than-is-opened"
  (interactive)
  (let ((fl (frame-list)))
    (mapc (lambda (frame)
            (if (string-equal "initial_terminal" (terminal-name frame))
                (delete frame fl))) fl)
    (setq fl (delete (selected-frame) fl))
    (if (wg-current-session t)
        (wg-set-session-parameter (wg-current-session t)
                                  'frame-list
                                  (mapcar 'wg-frame-to-wconfig fl)))))


(defun wg-reload-session ()
  "Reload current workgroups session."
  (interactive)
  (let ((file (or (wg-determine-session-save-file-name)
                  wg-session-file)))
    (when (file-exists-p file)
      (condition-case err
          (wg-open-session wg-session-file)
        (progn
          (wg-create-first-wg)
          (error (message "Error finding session-file: %s" err)))))
    (wg-create-first-wg)))

(defun wg-save-session-on-emacs-exit ()
  "Call `wg-save-session-on-exit' with `wg-emacs-exit-save-behavior'.
Added to `kill-emacs-query-functions'."
  (wg-save-session-on-exit wg-emacs-exit-save-behavior) t)

(defun wg-save-session-on-workgroups-mode-exit ()
  "Call `wg-save-session-on-exit' with `wg-workgroups-mode-exit-save-behavior'.
Called when `workgroups-mode' is turned off."
  (wg-save-session-on-exit wg-workgroups-mode-exit-save-behavior) t)


(defun wg-pickel-all-session-parameters (session)
  "Return a copy of SESSION after pickeling its
parameters and the parameters of all its workgroups."
  (let ((copy (wg-copy-session session)))
    (when (wg-session-parameters copy)
      (wg-asetf (wg-session-parameters copy) (wg-pickel it)))
    (wg-asetf (wg-session-workgroup-list copy)
              (cl-mapcar 'wg-pickel-workgroup-parameters it))
    copy))

(defun wg-unpickel-session-parameters (session)
  "Return a copy of SESSION after unpickeling its
parameters and the parameters of all its workgroups."
  (let ((copy (wg-copy-session session)))
    (when (wg-session-parameters copy)
      (wg-asetf (wg-session-parameters copy) (wg-unpickel it)))
    (wg-asetf (wg-session-workgroup-list copy)
              (cl-mapcar 'wg-unpickel-workgroup-parameters it))
    copy))


(provide 'workgroups-session)
;;; workgroups-session.el ends here
