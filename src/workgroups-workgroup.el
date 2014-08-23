;;; workgroups-workgroup.el --- workgroup functions
;;; Commentary:
;;; Code:

(require 'ring)
(require 'workgroups-wconfig)
(require 'workgroups-minibuffer)

;;
;; Variables
;;
(defvar wg-deactivation-list nil
  "A stack of workgroups that are currently being switched away from.
Used to avoid associating the old workgroup's buffers with the
new workgroup during a switch.")

(defcustom wg-confirm-on-get-workgroup-create nil
  "Non-nil means request confirmation before creating a new
workgroup when `wg-get-workgroup-create' is called with a string
that doesn't name an existing workgroup."
  :type 'boolean
  :group 'workgroups)

(defun wg-flag-workgroup-modified (workgroup)
  "Set WORKGROUP's and the current session's modified flags."
  (when wg-flag-modified
    (setf (wg-workgroup-modified workgroup) t)
    (setf (wg-session-modified (wg-current-session)) t)))

(defun wg-current-workgroup (&optional noerror frame)
  "Return the current workgroup in FRAME, or error unless NOERROR."
  (or wg-current-workgroup
      (aif (frame-parameter frame 'wg-current-workgroup-uid)
          (wg-find-workgroup-by :uid it noerror)
        (unless noerror (error "No current workgroup in this frame")))))

(defun wg-previous-workgroup (&optional noerror frame)
  "Return the previous workgroup in FRAME, or error unless NOERROR."
  (aif (frame-parameter frame 'wg-previous-workgroup-uid)
      (wg-find-workgroup-by :uid it noerror)
    (unless noerror (error "No previous workgroup in this frame"))))

(defun wg-set-current-workgroup (workgroup &optional frame)
  "Set the current workgroup to WORKGROUP.
WORKGROUP should be a workgroup or nil."
  (set-frame-parameter frame 'wg-current-workgroup-uid
                       (when workgroup (wg-workgroup-uid workgroup))))

(defun wg-set-previous-workgroup (workgroup &optional frame)
  "Set the previous workgroup to WORKGROUP.
WORKGROUP should be a workgroup or nil."
  (set-frame-parameter frame 'wg-previous-workgroup-uid
                       (when workgroup (wg-workgroup-uid workgroup))))

(defun wg-current-workgroup-p (workgroup &optional frame)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup t frame)
    (eq workgroup it)))

(defun wg-previous-workgroup-p (workgroup &optional frame)
  "Return t when WORKGROUP is the previous workgroup, nil otherwise."
  (wg-awhen (wg-previous-workgroup t frame)
    (eq workgroup it)))

(defmacro wg-with-current-workgroup (workgroup &rest body)
  "Execute forms in BODY with WORKGROUP temporarily current.
WORKGROUP should be any workgroup identifier accepted by
`wg-get-workgroup'.  The value returned is the last form
in BODY."
  (declare (indent 1))
  `(let ((wg-current-workgroup (wg-get-workgroup ,workgroup)))
     ,@body))

(defun wg-get-workgroup (obj &optional noerror)
  "Return a workgroup from OBJ.
If OBJ is a workgroup, return it.
If OBJ is a string, return the workgroup named OBJ, or error unless NOERROR.
If OBJ is nil, return the current workgroup, or error unless NOERROR."
  (cond ((wg-workgroup-p obj) obj)
        ((stringp obj) (wg-find-workgroup-by :name obj noerror))
        ((null obj) (wg-current-workgroup noerror))
        (t (error "Can't get workgroup from type:: %S" (type-of obj)))))



;;; workgroup parameters
;;
;; Quick test:
;; (wg-workgroup-parameters (wg-current-workgroup))
;; (wg-set-workgroup-parameter (wg-current-workgroup) 'test1 t)
;; (wg-workgroup-parameter (wg-current-workgroup) 'test1)
(defun wg-workgroup-parameter (workgroup parameter &optional default)
  "Return WORKGROUP's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
WORKGROUP should be accepted by `wg-get-workgroup'."
  (wg-aget (wg-workgroup-parameters (wg-get-workgroup workgroup))
           parameter default))

(defun wg-set-workgroup-parameter (workgroup parameter value)
  "Set WORKGROUP's value of PARAMETER to VALUE.
WORKGROUP should be a value accepted by `wg-get-workgroup'.
Return VALUE."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-set-parameter (wg-workgroup-parameters workgroup) parameter value)
    (wg-flag-workgroup-modified workgroup)
    value))

(defun wg-remove-workgroup-parameter (workgroup parameter)
  "Remove PARAMETER from WORKGROUP's parameters."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-flag-workgroup-modified workgroup)
    (wg-asetf (wg-workgroup-parameters workgroup) (wg-aremove it parameter))))

(defun wg-workgroup-local-value (variable &optional workgroup)
  "Return the value of VARIABLE in WORKGROUP.
WORKGROUP nil defaults to the current workgroup.  If there is no
current workgroup, or if VARIABLE does not have a workgroup-local
binding in WORKGROUP, resolve VARIABLE with `wg-session-local-value'."
  (let ((workgroup (wg-get-workgroup workgroup t)))
    (if (not workgroup) (wg-session-local-value variable)
      (let* ((undefined (cl-gensym))
             (value (wg-workgroup-parameter workgroup variable undefined)))
        (if (not (eq value undefined)) value
          (wg-session-local-value variable))))))

(defalias 'wg-local-value 'wg-workgroup-local-value)


;;; workgroup saved wconfigs

(defun wg-workgroup-saved-wconfig-names (workgroup)
  "Return a new list of the names of all WORKGROUP's saved wconfigs."
  (mapcar 'wg-wconfig-name (wg-workgroup-saved-wconfigs workgroup)))

(defun wg-workgroup-get-saved-wconfig (workgroup wconfig-or-name)
  "Return the wconfig in WORKGROUP's saved wconfigs named WCONFIG-OR-NAME.
WCONFIG-OR-NAME must be either a string or a wconfig.  If
WCONFIG-OR-NAME is a string and there is no saved wconfig with
that name, return nil.  If WCONFIG-OR-NAME is a wconfig, and it
is a member of WORKGROUP's saved wconfigs, return is as given.
Otherwise return nil."
  (let ((wconfigs (wg-workgroup-saved-wconfigs workgroup)))
    (cl-etypecase wconfig-or-name
      (wg-wconfig (car (memq wconfig-or-name wconfigs)))
      (string (cl-find wconfig-or-name wconfigs
                       :key 'wg-wconfig-name
                       :test 'string=)))))

(defun wg-workgroup-save-wconfig (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's saved wconfigs.  WCONFIG must have
a name.  If there's already a wconfig with the same name in
WORKGROUP's saved wconfigs, replace it."
  (let ((name (wg-wconfig-name wconfig)))
    (unless name (error "Attempt to save a nameless wconfig"))
    (setf (wg-workgroup-modified workgroup) t)
    (wg-asetf (wg-workgroup-saved-wconfigs workgroup)
              (cons wconfig (cl-remove name it
                                       :key 'wg-wconfig-name
                                       :test 'string=)))))

(defun wg-workgroup-kill-saved-wconfig (workgroup wconfig-or-name)
  "Delete WCONFIG-OR-NAME from WORKGROUP's saved wconfigs.
WCONFIG-OR-NAME is resolved with `wg-workgroup-get-saved-wconfig'."
  (wg-when-let ((wconfig (wg-workgroup-get-saved-wconfig
                          workgroup wconfig-or-name)))
    (wg-asetf (wg-workgroup-saved-wconfigs workgroup) (remq wconfig it)
              (wg-workgroup-modified workgroup) t)))



(defun wg-workgroup-base-wconfig-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's working wconfig."
  (wg-wconfig-buf-uids (wg-workgroup-base-wconfig workgroup)))

(defun wg-workgroup-saved-wconfigs-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's base wconfig."
  (cl-reduce 'wg-string-list-union
             (wg-workgroup-saved-wconfigs workgroup)
             :key 'wg-wconfig-buf-uids))

(defun wg-workgroup-all-wconfig-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's wconfigs."
  (cl-union (wg-workgroup-base-wconfig-buf-uids workgroup)
            (wg-workgroup-saved-wconfigs-buf-uids workgroup)
            :test 'string=))

(defun wg-workgroup-all-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP."
  (cl-reduce 'wg-string-list-union
             (list (wg-workgroup-base-wconfig-buf-uids workgroup)
                   (wg-workgroup-saved-wconfigs-buf-uids workgroup))))



;;; workgroup restoration

(defun wg-restore-workgroup (workgroup)
  "Restore WORKGROUP in `selected-frame'."
  (let (wg-flag-modified)
    (wg-restore-wconfig-undoably (wg-workgroup-working-wconfig workgroup) t)))


(defun wg-workgroup-list-or-error (&optional noerror)
  "Return the value of `wg-current-session's :workgroup-list slot.
Or scream unless NOERROR."
  (or (wg-workgroup-list)
      (unless noerror
        (error "No workgroups are defined"))))

(defun wg-find-workgroup-by (slotkey value &optional noerror)
  "Return the workgroup on which ACCESSOR returns VALUE or error."
  (let ((accessor (cl-ecase slotkey
                    (:name 'wg-workgroup-name)
                    (:uid  'wg-workgroup-uid))))
    (or (cl-find value (wg-workgroup-list-or-error noerror) :test 'equal :key accessor)
        (unless noerror
          (error "No are no workgroups with a %S of %S"
                 accessor value)))))

(defun wg-first-workgroup ()
  "Return a first workgroup."
  (car (wg-workgroup-list-or-error)))

(defun wg-cyclic-nth-from-workgroup (workgroup &optional n)
  "Return the workgroup N places from WORKGROUP in `wg-workgroup-list'."
  (wg-cyclic-nth-from-elt workgroup (wg-workgroup-list-or-error) (or n 1)))


(defun wg-read-workgroup-name (&optional require-match)
  "Read a workgroup with `wg-completing-read'."
  (wg-completing-read
   "Workgroup: " (wg-workgroup-names) nil require-match nil nil
   (wg-awhen (wg-current-workgroup t) (wg-workgroup-name it))))

(defun wg-new-default-workgroup-name ()
  "Return a new, unique, default workgroup name."
  (let ((names (wg-workgroup-names t)) (index -1) result)
    (while (not result)
      (let ((new-name (format "wg%s" (cl-incf index))))
        (unless (member new-name names)
          (setq result new-name))))
    result))

(defun wg-unique-workgroup-name-p (new-name)
  "Return t if NEW-NAME is unique in `wg-workgroup-list', nil otherwise."
  (cl-every (lambda (existing-name) (not (equal new-name existing-name)))
            (wg-workgroup-names t)))

(defun wg-read-saved-wconfig-name (workgroup &optional prompt require-match)
  "Read the name of a saved wconfig, completing on the names of
WORKGROUP's saved wconfigs."
  (wg-completing-read
   (or prompt "Saved wconfig name: ")
   (wg-workgroup-saved-wconfig-names workgroup)
   nil require-match))

(defun wg-read-saved-wconfig (workgroup)
  "Read the name of and return one of WORKGROUP's saved wconfigs."
  (wg-workgroup-get-saved-wconfig
   workgroup (wg-read-saved-wconfig-name workgroup nil t)))


;;; workgroup-list reorganization commands

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-swap-workgroups-in-workgroup-list
   (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-message
    (:cmd "Swapped:  ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-left (&optional workgroup n)
  "Offset WORKGROUP leftward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n -1))
  (wg-fontified-message
    (:cmd "Offset left: ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-right (&optional workgroup n)
  "Offset WORKGROUP rightward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n 1))
  (wg-fontified-message
    (:cmd "Offset right: ")
    (wg-workgroup-list-display)))


;;; undo/redo commands

(defun wg-undo-wconfig-change (&optional workgroup)
  "Undo a change to the current workgroup's window-configuration."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (undid? (wg-workgroup-offset-position-in-undo-list workgroup 1)))
    (wg-fontified-message
      (:cmd "Undo")
      (:cur (if undid? "" "  No more undo info")))))

(defun wg-redo-wconfig-change (&optional workgroup)
  "Redo a change to the current workgroup's window-configuration."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (redid? (wg-workgroup-offset-position-in-undo-list workgroup -1)))
    (wg-fontified-message
      (:cmd "Redo")
      (:cur (if redid? "" "  No more redo info")))))

(defun wg-undo-once-all-workgroups ()
  "Do what the name says.  Useful for instance when you
accidentally call `wg-revert-all-workgroups' and want to return
all workgroups to their un-reverted state."
  (interactive)
  (mapc 'wg-undo-wconfig-change (wg-workgroup-list-or-error))
  (wg-message "Undid once on all workgroups."))

(defun wg-redo-once-all-workgroups ()
  "Do what the name says.  Probably useless.  Included for
symetry with `wg-undo-once-all-workgroups'."
  (interactive)
  (mapc 'wg-redo-wconfig-change (wg-workgroup-list-or-error))
  (wg-message "Redid once on all workgroups."))



;;; window-tree commands
;;
;; TODO: These are half-hearted.  Clean them up; allow specification of the
;; window-tree depth at which to operate; add complex window creation commands;
;; and add window splitting, deletion and locking commands.

(defun wg-reverse-frame-horizontally (&optional workgroup)
  "Reverse the order of all horizontally split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup)))))

(defun wg-reverse-frame-vertically (&optional workgroup)
  "Reverse the order of all vertically split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    t)))

(defun wg-reverse-frame-horizontally-and-vertically (&optional workgroup)
  "Reverse the order of all wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    'both)))


;;; misc commands

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list nil (wg-read-new-workgroup-name "New name: ")))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (oldname (wg-workgroup-name workgroup)))
    (setf (wg-workgroup-name workgroup) newname)
    (wg-flag-workgroup-modified workgroup)
    (wg-fontified-message
      (:cmd "Renamed: ")
      (:cur oldname)
      (:msg " to ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-reset (&optional force)
  "Reset Workgroups.
Resets all frame parameters, buffer-local vars, the current
Workgroups session object, etc."
  (interactive "P")
  (unless (or force wg-no-confirm-on-destructive-operation
              (y-or-n-p "Really reset Workgroups? "))
    (error "Canceled"))
  (wg-reset-internal)
  (wg-fontified-message (:cmd "Reset: ") (:msg "Workgroups")))


(defun wg-query-and-save-if-modified ()
  "Query for save when `wg-modified-p'."
  (or (not (wg-modified-p))
      (when (y-or-n-p "Save modified workgroups? ")
        (wg-save-session))))


;;; workgroup creation commands

(defun wg-create-workgroup (name &optional blank)
  "Create and add a workgroup named NAME.
Optional argument BLANK non-nil (set interactively with a prefix
arg) means use a blank, one window window-config.  Otherwise use
the current window-configuration.  Keep in mind that even though
the current window-config may be used, other parameters of the
current workgroup are not copied to the created workgroup.  For
that, use `wg-clone-workgroup'."
  (interactive (list (wg-read-new-workgroup-name) current-prefix-arg))
  (wg-switch-to-workgroup (wg-make-and-add-workgroup name blank))
  (wg-fontified-message
    (:cmd "Created: ") (:cur name) "  " (wg-workgroup-list-display)))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME.
Keep in mind that only WORKGROUP's top-level alist structure is
copied, so destructive operations on the keys or values of
WORKGROUP will be reflected in the clone, and vice-versa.  Be
safe -- don't mutate them."
  (interactive (list nil (wg-read-new-workgroup-name)))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (clone (wg-copy-workgroup workgroup)))
    (setf (wg-workgroup-name clone) name
          (wg-workgroup-uid clone) (wg-generate-uid))
    (when (wg-check-and-add-workgroup clone)
      (wg-flag-workgroup-modified clone))
    (wg-set-workgroup-working-wconfig
     clone (wg-workgroup-working-wconfig workgroup))
    (wg-switch-to-workgroup clone)
    (wg-fontified-message
      (:cmd "Cloned: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " to ")
      (:cur name) "  "
      (wg-workgroup-list-display))))



;;; workgroup switching commands
(defun wg-switch-to-workgroup (workgroup &optional noerror)
  "Switch to WORKGROUP.
NOERROR means fail silently."
  (interactive (list (wg-read-workgroup-name)))

  (fset 'buffer-list wg-buffer-list-original)

  ;; Mark if ECB is active
  (if (wg-current-workgroup t)
      (wg-set-workgroup-parameter (wg-current-workgroup t) 'ecb (and (boundp 'ecb-minor-mode)
                                                                     ecb-minor-mode)))
  ;;(wg-set-workgroup-parameter (wg-current-workgroup t) 'ecb-win-config (ecb-current-window-configuration))
  ;; (type-of (ecb-current-window-configuration))
  ;; (type-of (car (ecb-current-window-configuration)))
  ;; (type-of (car (nthcdr 3 (ecb-current-window-configuration))))
  ;; (wg-pickelable-or-error (ecb-current-window-configuration))
  ;;(ecb-current-window-configuration)
  ;;)

  (let ((workgroup (wg-get-workgroup-create workgroup))
        (current (wg-current-workgroup t)))
    (when (and (eq workgroup current) (not noerror))
      (error "Already on: %s" (wg-workgroup-name current)))
    (when current (push current wg-deactivation-list))
    (unwind-protect
        (progn
          ;; Before switching - turn off ECB
          ;; https://github.com/pashinin/workgroups2/issues/34
          (if (and (boundp 'ecb-minor-mode)
                   (boundp 'ecb-frame)
                   (fboundp 'ecb-deactivate)
                   ecb-minor-mode
                   (equal ecb-frame (selected-frame)))
              (let ((ecb-split-edit-window-after-start 'before-deactivation))
                (ecb-deactivate)))

          (wg-restore-workgroup workgroup)
          (wg-set-previous-workgroup current)
          (wg-set-current-workgroup workgroup)

          ;; Save "last-workgroup" to the session params
          (if (and (wg-current-session t)
                   (wg-current-workgroup t))
              (wg-set-session-parameter (wg-current-session t)
                                        'last-workgroup
                                        (wg-workgroup-name (wg-current-workgroup))))

          ;; If a workgroup had ECB - turn it on
          (if (and (boundp 'ecb-minor-mode)
                   (not ecb-minor-mode)
                   (fboundp 'ecb-activate)
                   (wg-workgroup-parameter (wg-current-workgroup t) 'ecb nil))
              (let ((ecb-split-edit-window-after-start 'before-deactivation))
                (ecb-activate)))
          ;;(ecb-last-window-config-before-deactivation
          ;; (wg-workgroup-parameter (wg-current-workgroup t) 'ecb-win-config nil)))

          (run-hooks 'wg-switch-to-workgroup-hook)

          (if wg-mess-with-buffer-list
              (fset 'buffer-list wg-buffer-list-function))
          (wg-fontified-message
            (:cmd "Switched: ")
            (wg-workgroup-name (wg-current-workgroup t))
            ))
      (when current (pop wg-deactivation-list)))))

(defun wg-switch-to-workgroup-other-frame (workgroup &optional n)
  "Switch to WORKGROUP in the frame N places cyclically from `selected-frame'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list (wg-read-workgroup-name) current-prefix-arg))
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (wg-switch-to-workgroup workgroup)))

(defun wg-switch-to-workgroup-at-index (index)
  "Switch to the workgroup at INDEX in `wg-workgroup-list'."
  (interactive (list (or current-prefix-arg (wg-read-workgroup-index))))
  (let ((wl (wg-workgroup-list-or-error)))
    (wg-switch-to-workgroup
     (or (nth index wl) (error "There are only %d workgroups" (length wl))))))

(cl-macrolet
    ((define-range-of-switch-to-workgroup-at-index (num)
       `(progn
          ,@(wg-docar (i (wg-range 0 num))
              `(defun ,(intern (format "wg-switch-to-workgroup-at-index-%d" i)) ()
                 ,(format "Switch to the workgroup at index %d." i)
                 (interactive)
                 (wg-switch-to-workgroup-at-index ,i))))))
  (define-range-of-switch-to-workgroup-at-index 10))

(defun wg-switch-to-cyclic-nth-from-workgroup (workgroup n)
  "Switch N workgroups cyclically from WORKGROUP in `wg-workgroup-list.'"
  (let ((workgroup-list (wg-workgroup-list-or-error))
        (workgroup (wg-get-workgroup workgroup t)))
    (wg-switch-to-workgroup
     (cond ((not workgroup) (car workgroup-list))
           ((= 1 (length workgroup-list)) (error "There's only one workgroup"))
           (t (wg-cyclic-nth-from-workgroup workgroup n))))))

(defun wg-switch-to-workgroup-left (&optional workgroup n)
  "Switch to the workgroup (- N) places from WORKGROUP in `wg-workgroup-list'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list nil current-prefix-arg))
  (wg-switch-to-cyclic-nth-from-workgroup workgroup (- (or n 1))))

(defun wg-switch-to-workgroup-right (&optional workgroup n)
  "Switch to the workgroup N places from WORKGROUP in `wg-workgroup-list'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list nil current-prefix-arg))
  (wg-switch-to-cyclic-nth-from-workgroup workgroup (or n 1)))

(defun wg-switch-to-previous-workgroup ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch-to-workgroup (wg-previous-workgroup)))



;;; workgroup killing commands

(defun wg-wconfig-kill-ring ()
  "Return `wg-wconfig-kill-ring', creating it first if necessary."
  (or wg-wconfig-kill-ring
      (setq wg-wconfig-kill-ring (make-ring wg-wconfig-kill-ring-max))))

(defun wg-add-to-wconfig-kill-ring (wconfig)
  "Add WCONFIG to `wg-wconfig-kill-ring'."
  (ring-insert (wg-wconfig-kill-ring) wconfig))

(defun wg-kill-workgroup (&optional workgroup)
  "Kill WORKGROUP, saving its working-wconfig to the kill ring."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (to (or (wg-previous-workgroup t)
                 (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-delete-workgroup workgroup)
    (if (eq workgroup to) (wg-restore-wconfig (wg-make-blank-wconfig))
      (wg-switch-to-workgroup to))
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-ring-save-base-wconfig (&optional workgroup)
  "Save WORKGROUP's base wconfig to the kill ring."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-base-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ") (:cur (wg-workgroup-name workgroup))
      (:cur "'s ") (:msg "base wconfig to the kill ring"))))

(defun wg-kill-ring-save-working-wconfig (&optional workgroup)
  "Save WORKGROUP's working-wconfig to `wg-wconfig-kill-ring'."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ") (:cur (wg-workgroup-name workgroup))
      (:cur "'s ") (:msg "working-wconfig to the kill ring"))))

(defun wg-yank-wconfig ()
  "Restore a wconfig from `wg-wconfig-kill-ring'.
Successive yanks restore wconfigs sequentially from the kill
ring, starting at the front."
  (interactive)
  (when (zerop (ring-length (wg-wconfig-kill-ring)))
    (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-wconfig)) 0
               (1+ (or (get 'wg-yank-wconfig :position) 0)))))
    (put 'wg-yank-wconfig :position pos)
    (wg-restore-wconfig-undoably (ring-ref (wg-wconfig-kill-ring) pos))
    (wg-fontified-message
      (:cmd "Yanked: ")
      (:msg (format "%S" pos)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-workgroup-and-buffers (&optional workgroup)
  "Kill WORKGROUP and the buffers in its working-wconfig."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (bufs (save-window-excursion
                 (wg-restore-workgroup workgroup)
                 (mapcar #'window-buffer (window-list)))))
    (wg-kill-workgroup workgroup)
    (mapc #'kill-buffer bufs)
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " and its buffers ") "\n"
      (wg-workgroup-list-display))))

(defun wg-delete-other-workgroups (&optional workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (unless (or wg-no-confirm-on-destructive-operation
                (y-or-n-p "Really delete all other workgroups? "))
      (error "Cancelled"))
    (dolist (w (wg-workgroup-list-or-error))
      (unless (eq w workgroup)
        (wg-delete-workgroup w)))
    (unless (wg-current-workgroup-p workgroup)
      (wg-switch-to-workgroup workgroup))
    (wg-fontified-message
      (:cmd "Deleted: ")
      (:msg "All workgroups but ")
      (:cur (wg-workgroup-name workgroup)))))



;;; workgroup updating and reverting commands

(defun wg-revert-workgroup (&optional workgroup)
  "Restore WORKGROUP's window configuration to its state at the last save."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (base-wconfig (wg-workgroup-base-wconfig workgroup)))
    (if (wg-current-workgroup-p workgroup)
        (wg-restore-wconfig-undoably base-wconfig)
      (wg-add-wconfig-to-undo-list workgroup base-wconfig))
    (wg-fontified-message
      (:cmd "Reverted: ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base wconfigs.
Only workgroups' working-wconfigs in `selected-frame' are
reverted."
  (interactive)
  (mapc #'wg-revert-workgroup (wg-workgroup-list-or-error))
  (wg-fontified-message
    (:cmd "Reverted: ")
    (:msg "All")))



;;; workgroup working-wconfig and wconfig undo/redo

(defun wg-workgroup-state-table (&optional frame)
  "Return FRAME's workgroup table, creating it first if necessary."
  (or (frame-parameter frame 'wg-workgroup-state-table)
      (let ((wtree (make-hash-table :test 'equal)))
        (set-frame-parameter frame 'wg-workgroup-state-table wtree)
        wtree)))

(defun wg-get-workgroup-state (workgroup &optional frame)
  "Return WORKGROUP's state table in a FRAME."
  (let ((uid (wg-workgroup-uid workgroup))
        (state-table (wg-workgroup-state-table frame)))
    (or (gethash uid state-table)
        (let ((wgs (wg-make-workgroup-state
                    :undo-pointer 0
                    :undo-list
                    (list (or (wg-workgroup-selected-frame-wconfig workgroup)
                              (wg-workgroup-base-wconfig workgroup))))))
          (puthash uid wgs state-table)
          wgs))))

(defmacro wg-with-undo (workgroup spec &rest body)
  "Bind WORKGROUP's undo state to SPEC and eval BODY."
  (declare (indent 2))
  (wg-dbind (state undo-pointer undo-list) spec
    `(let* ((,state (wg-get-workgroup-state ,workgroup))
            (,undo-pointer (wg-workgroup-state-undo-pointer ,state))
            (,undo-list (wg-workgroup-state-undo-list ,state)))
       ,@body)))

(defun wg-flag-just-exited-minibuffer ()
  "Added to `minibuffer-exit-hook'."
  (setq wg-just-exited-minibuffer t))

(defun wg-flag-window-configuration-changed ()
  "Set `wg-window-configuration-changed' to t.
But only if not the minibuffer was just exited.  Added to
`window-configuration-change-hook'."
  (if wg-just-exited-minibuffer
      (setq wg-just-exited-minibuffer nil)
    (setq wg-window-configuration-changed t)))

(defun wg-unflag-undoify-window-configuration-change ()
  "Set `wg-undoify-window-configuration-change' to nil, exempting
from undoification any window-configuration changes caused by the
current command."
  (setq wg-undoify-window-configuration-change nil))

(defun wg-set-workgroup-working-wconfig (workgroup wconfig)
  "Set the working-wconfig of WORKGROUP to WCONFIG."
  (wg-flag-workgroup-modified workgroup)
  (setf (wg-workgroup-selected-frame-wconfig workgroup) wconfig)
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (setcar (nthcdr undo-pointer undo-list) wconfig)))

(defun wg-add-wconfig-to-undo-list (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's undo list, truncating its future if necessary."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (let ((undo-list (cons nil (nthcdr undo-pointer undo-list))))
      (wg-awhen (nthcdr wg-wconfig-undo-list-max undo-list) (setcdr it nil))
      (setf (wg-workgroup-state-undo-list state) undo-list))
    (setf (wg-workgroup-state-undo-pointer state) 0))
  (wg-set-workgroup-working-wconfig workgroup wconfig))

(defun wg-workgroup-working-wconfig (workgroup &optional noupdate)
  "Return WORKGROUP's working-wconfig.
If WORKGROUP is the current workgroup in `selected-frame', and
NOUPDATE is nil, set its working wconfig in `selected-frame' to
`wg-current-wconfig' and return the updated wconfig.  Otherwise
return WORKGROUP's current undo state."
  (if (and (not noupdate) (wg-current-workgroup-p workgroup))
      (wg-set-workgroup-working-wconfig workgroup (wg-current-wconfig))
    (wg-with-undo workgroup (state undo-pointer undo-list)
      (nth undo-pointer undo-list))))

(defun wg-update-current-workgroup-working-wconfig ()
  "Update `selected-frame's current workgroup's working-wconfig with `wg-current-wconfig'."
  (wg-awhen (wg-current-workgroup t)
    (wg-set-workgroup-working-wconfig it (wg-current-wconfig))))

(defun wg-restore-wconfig-undoably (wconfig &optional noundo)
  "Restore WCONFIG in `selected-frame', saving undo information.
Skip undo when NOUNDO."
  (when noundo (wg-unflag-undoify-window-configuration-change))
  (wg-update-current-workgroup-working-wconfig)
  (wg-restore-wconfig wconfig))

(defun wg-workgroup-offset-position-in-undo-list (workgroup increment)
  "Increment WORKGROUP's undo-pointer by INCREMENT.
Also restore the wconfig at the incremented undo-pointer if
WORKGROUP is current."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (let ((new-pointer (+ undo-pointer increment)))
      (when (wg-within new-pointer 0 (length undo-list))
        (when (wg-current-workgroup-p workgroup)
          (wg-restore-wconfig-undoably (nth new-pointer undo-list) t))
        (setf (wg-workgroup-state-undo-pointer state) new-pointer)))))

(defun wg-undoify-window-configuration-change ()
  "Conditionally `wg-add-wconfig-to-undo-list'.
Added to `post-command-hook'."
  (when (and
         wg-window-configuration-changed         ;; When the window config has changed,
         wg-undoify-window-configuration-change  ;; and undoification is still on for the current command
         (wg-minibuffer-inactive-p))             ;; and the change didn't occur while the minibuffer is active,
    (wg-when-let ((workgroup (wg-current-workgroup t)))  ;; and there's a current workgroup,
      ;; add the current wconfig to that workgroup's undo list:
      (wg-add-wconfig-to-undo-list workgroup (wg-current-wconfig))))
  ;; Reset all flags no matter what:
  (setq wg-window-configuration-changed nil
        wg-undoify-window-configuration-change t
        wg-already-updated-working-wconfig nil))

(defun wg-update-working-wconfig-hook ()
  "Used in before advice on all functions that trigger `window-configuration-change-hook'.
To save up to date undo info before the change."
  (when (and (not wg-already-updated-working-wconfig)
             (wg-minibuffer-inactive-p))
    (wg-update-current-workgroup-working-wconfig)
    (setq wg-already-updated-working-wconfig t)))


(defun wg-workgroup-gc-buf-uids (workgroup)
  "Remove buf uids from WORKGROUP that have no referent in `wg-buf-list'."
  (wg-asetf (wg-workgroup-strong-buf-uids workgroup)
            (cl-remove-if-not 'wg-find-buf-by-uid it)
            (wg-workgroup-weak-buf-uids workgroup)
            (cl-remove-if-not 'wg-find-buf-by-uid it)))

(defun wg-gc-buf-uids ()
  "Remove from all workgroups those buf uids that have no referent in `wg-buf-list'."
  (mapc 'wg-workgroup-gc-buf-uids (wg-workgroup-list)))



(defun wg-display-internal (elt-fn list)
  "Return display string built by calling ELT-FN on each element of LIST."
  (let ((div (wg-add-face :div wg-list-display-decor-divider))
        (wwidth (window-width (minibuffer-window)))
        (i -1)
        (str))
    (setq str
          (wg-fontify
            (:brace wg-list-display-decor-left-brace)
            (if (not list) (funcall elt-fn nil nil)
              (wg-doconcat (elt list div) (funcall elt-fn elt (cl-incf i))))
            (:brace wg-list-display-decor-right-brace)))
    ;; (subseq str 0 wwidth)
    ))

(defun wg-workgroup-list-display (&optional workgroup-list)
  "Return the Workgroups list display string.
The string contains the names of all workgroups in `wg-workgroup-list',
decorated with faces, dividers and strings identifying the
current and previous workgroups."
  (wg-display-internal 'wg-workgroup-display
                       (or workgroup-list (wg-workgroup-list))))

(defun wg-create-first-wg ()
  "Create a first workgroup if needed."
  (if (and workgroups-mode
           wg-session-load-on-start
           (= (length (wg-workgroup-list)) 0))
      (wg-create-workgroup wg-first-wg-name)))


(defun wg-pickel-workgroup-parameters (workgroup)
  "Return a copy of WORKGROUP after pickeling its parameters.
If WORKGROUP's parameters are non-nil, otherwise return
WORKGROUP."
  (if (not (wg-workgroup-parameters workgroup)) workgroup
    (let ((copy (wg-copy-workgroup workgroup)))
      (wg-asetf (wg-workgroup-parameters copy) (wg-pickel it))
      copy)))

(defun wg-unpickel-workgroup-parameters (workgroup)
  "If WORKGROUP's parameters are non-nil, return a copy of
WORKGROUP after unpickeling its parameters. Otherwise return
WORKGROUP."
  (if (not (wg-workgroup-parameters workgroup)) workgroup
    (let ((copy (wg-copy-workgroup workgroup)))
      (wg-asetf (wg-workgroup-parameters copy) (wg-unpickel it))
      copy)))

;;; workgroup-list ops

(defun wg-delete-workgroup (workgroup)
  "Remove WORKGROUP from `wg-workgroup-list'.
Also delete all references to it by `wg-workgroup-state-table',
`wg-current-workgroup' and `wg-previous-workgroup'."
  (dolist (frame (frame-list))
    (remhash (wg-workgroup-uid workgroup) (wg-workgroup-state-table frame))
    (when (wg-current-workgroup-p workgroup frame)
      (wg-set-current-workgroup nil frame))
    (when (wg-previous-workgroup-p workgroup frame)
      (wg-set-previous-workgroup nil frame)))
  (setf (wg-workgroup-list) (remove workgroup (wg-workgroup-list-or-error)))
  (setf (wg-session-modified (wg-current-session)) t)
  workgroup)

(defun wg-add-workgroup (workgroup &optional index)
  "Add WORKGROUP to `wg-workgroup-list' at INDEX or the end.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-find-workgroup-by :name (wg-workgroup-name workgroup) t)
    (unless index (setq index (cl-position it (wg-workgroup-list-or-error))))
    (wg-delete-workgroup it))
  (wg-asetf (wg-workgroup-list)
            (wg-insert-before workgroup it (or index (length it))))
  (setf (wg-session-modified (wg-current-session)) t)
  workgroup)

(defun wg-check-and-add-workgroup (workgroup)
  "Add WORKGROUP to `wg-workgroup-list'.
Ask to overwrite if a workgroup with the same name exists."
  (let ((name (wg-workgroup-name workgroup))
        (uid (wg-workgroup-uid workgroup)))
    (when (wg-find-workgroup-by :uid uid t)
      (error "A workgroup with uid %S already exists" uid))
    (when (wg-find-workgroup-by :name name t)
      (unless (or wg-no-confirm-on-destructive-operation
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add-workgroup workgroup))

(defun wg-make-and-add-workgroup (name &optional blank)
  "Create a workgroup named NAME with current `window-tree'.
If BLANK - then just scratch buffer.
Add it with `wg-check-and-add-workgroup'."
  (wg-check-and-add-workgroup
   (wg-make-workgroup
    :name name
    :base-wconfig (if blank (wg-make-blank-wconfig)
                    (wg-current-wconfig)))))

(defun wg-get-workgroup-create (workgroup)
  "Return the workgroup specified by WORKGROUP, creating a new one if needed.
If `wg-get-workgroup' on WORKGROUP returns a workgroup, return it.
Otherwise, if WORKGROUP is a string, create a new workgroup with
that name and return it.  Otherwise error."
  (or (wg-get-workgroup workgroup t)
      (if (stringp workgroup)
          (when (or (not wg-confirm-on-get-workgroup-create)
                    (y-or-n-p (format "%S doesn't exist.  Create it? "
                                      workgroup)))
            (wg-make-and-add-workgroup workgroup))
        ;; Call this again for its error message
        (wg-get-workgroup workgroup))))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-workgroup-list' by N."
  (let ((workgroup-list (wg-workgroup-list-or-error)))
    (unless (member workgroup workgroup-list)
      (error "Workgroup isn't present in `wg-workgroup-list'."))
    (setf (wg-workgroup-list) (wg-cyclic-offset-elt workgroup workgroup-list n)
          (wg-session-modified (wg-current-session)) t)))

(defun wg-swap-workgroups-in-workgroup-list (workgroup1 workgroup2)
  "Swap the positions of WORKGROUP1 and WORKGROUP2 in `wg-workgroup-list'."
  (let ((workgroup-list (wg-workgroup-list-or-error)))
    (when (eq workgroup1 workgroup2)
      (error "Can't swap a workgroup with itself"))
    (unless (and (memq workgroup1 workgroup-list)
                 (memq workgroup2 workgroup-list))
      (error "Both workgroups aren't present in `wg-workgroup-list'."))
    (setf (wg-workgroup-list) (wg-util-swap workgroup1 workgroup2 workgroup-list)
          (wg-session-modified (wg-current-session)) t)))


(provide 'workgroups-workgroup)
;;; workgroups-workgroup.el ends here
