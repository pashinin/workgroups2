;;; workgroups-wconfig.el --- WCONFIG
;;; Commentary:
;; This is a window-configuration, a buffer layout, whatever you call
;; it...  This is actually what you want to be saved and restored.
;; (Well, technically it is (window-tree + some frame parameters))
;;; Code:

(require 'workgroups-wtree)

(defun wg-frame-to-wconfig (&optional frame)
  "Return the serialization (a wg-wconfig) of Emacs frame FRAME.
FRAME nil defaults to `selected-frame'."
  (let* ((frame (or frame (selected-frame)))
         (fullscrn (frame-parameter frame 'fullscreen)))
    (wg-make-wconfig
     :left                  (frame-parameter frame 'left)
     :top                   (frame-parameter frame 'top)
     :width                 (frame-parameter frame 'width)
     :height                (frame-parameter frame 'height)
     :parameters            `((fullscreen . ,fullscrn))
     :vertical-scroll-bars  (frame-parameter frame 'vertical-scroll-bars)
     :scroll-bar-width      (frame-parameter frame 'scroll-bar-width)
     :wtree                 (wg-window-tree-to-wtree (window-tree frame))
     )))

(defun wg-current-wconfig ()
  "Return the current wconfig.
If `wg-current-wconfig' is non-nil, return it.  Otherwise return
`wg-frame-to-wconfig'."
  (or (frame-parameter nil 'wg-current-wconfig)
      (wg-frame-to-wconfig)))

(defmacro wg-with-current-wconfig (frame wconfig &rest body)
  "Eval BODY with WCONFIG current in FRAME.
FRAME nil defaults to `selected-frame'."
  (declare (indent 2))
  (wg-with-gensyms (frame-sym old-value)
    `(let* ((,frame-sym (or ,frame (selected-frame)))
            (,old-value (frame-parameter ,frame-sym 'wg-current-wconfig)))
       (unwind-protect
           (progn
             (set-frame-parameter ,frame-sym 'wg-current-wconfig ,wconfig)
             ,@body)
         (when (frame-live-p ,frame-sym)
           (set-frame-parameter ,frame-sym 'wg-current-wconfig ,old-value))))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new blank wconfig.
BUFFER or `wg-default-buffer' is visible in the only window."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-frame-to-wconfig)))

(defun wg-wconfig-move-window (wconfig offset)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (wg-asetf (wg-wconfig-wtree wconfig) (wg-wtree-move-window it offset))
  wconfig)


;;; base wconfig updating

(defun wg-update-working-wconfig-on-delete-frame (frame)
  "Update FRAME's current workgroup's working-wconfig before
FRAME is deleted, so we don't lose its state."
  (with-selected-frame frame
    (wg-update-current-workgroup-working-wconfig)))


(defun wg-wconfig-buf-uids (wconfig)
  "Return WCONFIG's wtree's `wg-wtree-buf-uids'."
  (if (not (wg-wconfig-wtree wconfig))
      (error "WTREE is nil in `wg-wconfig-buf-uids'!"))
  (wg-wtree-unique-buf-uids (wg-wconfig-wtree wconfig)))





(defun wg-wconfig-restore-frame-position (wconfig &optional frame)
  "Use WCONFIG to restore FRAME's position.
If frame is nil then `selected-frame'."
  (wg-when-let ((left (wg-wconfig-left wconfig))
                (top (wg-wconfig-top wconfig)))
    ;; Check that arguments are integers
    ;; Problem: https://github.com/pashinin/workgroups2/issues/15
    (if (and (integerp left)
             (integerp top))
        (set-frame-position frame left top))))

(defun wg-wconfig-restore-scroll-bars (wconfig)
  "Restore `selected-frame's scroll-bar settings from WCONFIG."
  (set-frame-parameter
   nil 'vertical-scroll-bars (wg-wconfig-vertical-scroll-bars wconfig))
  (set-frame-parameter
   nil 'scroll-bar-width (wg-wconfig-scroll-bar-width wconfig)))

;;(defun wg-wconfig-restore-fullscreen (wconfig)
;;  "Restore `selected-frame's fullscreen settings from WCONFIG."
;;  (set-frame-parameter
;;   nil 'fullscreen (wg-wconfig-parameters wconfig))
;;  )

(defun wg-scale-wconfigs-wtree (wconfig new-width new-height)
  "Scale WCONFIG's wtree with NEW-WIDTH and NEW-HEIGHT.
Return a copy WCONFIG's wtree scaled with `wg-scale-wtree' by the
ratio or NEW-WIDTH to WCONFIG's width, and NEW-HEIGHT to
WCONFIG's height."
  (wg-normalize-wtree
   (wg-scale-wtree
    (wg-wconfig-wtree wconfig)
    (/ (float new-width)  (wg-wconfig-width wconfig))
    (/ (float new-height) (wg-wconfig-height wconfig)))))

(defun wg-scale-wconfig-to-frame (wconfig)
  "Scale WCONFIG buffers to fit current frame size.
Return a scaled copy of WCONFIG."
  (interactive)
  (wg-scale-wconfigs-wtree wconfig
                           (frame-parameter nil 'width)
                           (frame-parameter nil 'height)))

(defun wg-frame-resize-and-position (wconfig &optional frame)
  "Apply WCONFIG's size and position to a FRAME."
  (interactive)
  (unless frame (setq frame (selected-frame)))
  (let* ((params (wg-wconfig-parameters wconfig))
         fullscreen)
    (set-frame-parameter frame 'fullscreen (if (assoc 'fullscreen params)
                                               (cdr (assoc 'fullscreen params))
                                             nil))
    (when (and wg-restore-frame-position
               (not (frame-parameter frame 'fullscreen)))
      (wg-wconfig-restore-frame-position wconfig frame))
    ))

(defun wg-restore-frame-size-position (wconfig &optional fs)
  "Smart-restore of frame size and position.

Depending on `wg-remember-frame-for-each-wg' frame parameters may
be restored for each workgroup.

If `wg-remember-frame-for-each-wg' is nil (by default) then
current frame parameters are saved/restored to/from first
workgroup. And frame parameters for all other workgroups are just
ignored.
"
  (interactive)
  (let* ((params (wg-wconfig-parameters wconfig))
         fullscreen)
    ;; Frame maximized / fullscreen / none
    (unless wg-remember-frame-for-each-wg
      (setq params (wg-wconfig-parameters (wg-workgroup-working-wconfig (wg-first-workgroup)))))
    (setq fullscreen (if (assoc 'fullscreen params)
                         (cdr (assoc 'fullscreen params))
                       nil))
    (when (and fs
               fullscreen
               (or wg-remember-frame-for-each-wg
                   (null (wg-current-workgroup t))))
      (set-frame-parameter nil 'fullscreen fullscreen)
      ;; I had bugs restoring maximized frame:
      ;; Frame could be maximized but buffers are not scaled to fit it.
      ;;
      ;; Maybe because of `set-frame-parameter' takes some time to finish and is async.
      ;; So I tried this and it helped
      (sleep-for 0 100))

    ;; Position
    (when (and wg-restore-frame-position
               wg-remember-frame-for-each-wg
               (not (frame-parameter nil 'fullscreen)))
      (wg-wconfig-restore-frame-position wconfig))
    ))


(defun wg-restore-frames ()
  "Try to recreate opened frames, take info from session's 'frame-list parameter."
  (interactive)
  (delete-other-frames)
  (when (wg-current-session t)
    (let ((fl (wg-session-parameter (wg-current-session t) 'frame-list nil))
          (frame (selected-frame)))
      (mapc (lambda (wconfig)
              (with-selected-frame (make-frame)
                ;;(wg-frame-resize-and-position wconfig)
                ;;(wg-restore-frame-size-position wconfig)
                ;;(wg-wconfig-restore-frame-position wconfig)
                (wg-restore-wconfig wconfig)
                )) fl)
      (select-frame-set-input-focus frame))))

;; FIXME: throw a specific error if the restoration was unsuccessful
(defun wg-restore-wconfig (wconfig &optional frame)
  "Restore a workgroup configuration WCONFIG in a FRAME.
Runs each time you're switching workgroups."
  (unless frame (setq frame (selected-frame)))
  (let ((wg-record-incorrectly-restored-bufs t)
        (wg-incorrectly-restored-bufs nil)
        (params (wg-wconfig-parameters wconfig))
        fullscreen)
    (wg-barf-on-active-minibuffer)
    (when wg-restore-scroll-bars
      (wg-wconfig-restore-scroll-bars wconfig))

    (when (null (wg-current-workgroup t))
      (set-frame-parameter frame 'fullscreen (if (assoc 'fullscreen params)
                                                 (cdr (assoc 'fullscreen params))
                                               nil)))

    ;; Restore frame position
    (when (and wg-restore-frame-position
               (not (frame-parameter nil 'fullscreen))
               (null (wg-current-workgroup t)))
      (wg-wconfig-restore-frame-position wconfig frame))

    ;; Restore buffers
    (wg-restore-window-tree (wg-scale-wconfig-to-frame wconfig))

    (when wg-incorrectly-restored-bufs
      (message "Unable to restore these buffers: %S\
If you want, restore them manually and try again."
               (mapcar 'wg-buf-name wg-incorrectly-restored-bufs)))))


;;; saved wconfig commands

(defun wg-save-wconfig ()
  "Save the current wconfig to the current workgroup's saved wconfigs."
  (interactive)
  (let* ((workgroup (wg-current-workgroup))
         (name (wg-read-saved-wconfig-name workgroup))
         (wconfig (wg-current-wconfig)))
    (setf (wg-wconfig-name wconfig) name)
    (wg-workgroup-save-wconfig workgroup wconfig)
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur name))))

(defun wg-restore-saved-wconfig ()
  "Restore one of the current workgroup's saved wconfigs in `selected-frame'."
  (interactive)
  (let ((workgroup (wg-current-workgroup)))
    (wg-restore-wconfig-undoably
     (wg-workgroup-get-saved-wconfig
      workgroup
      (ido-completing-read "Saved wconfig: "
                           (mapcar 'wg-wconfig-name (wg-workgroup-saved-wconfigs workgroup))
                           nil t)))))

(defun wg-kill-saved-wconfig ()
  "Kill one of the current workgroup's saved wconfigs.
Also add it to the wconfig kill-ring."
  (interactive)
  (let* ((workgroup (wg-current-workgroup))
         (wconfig (wg-read-saved-wconfig workgroup)))
    (wg-workgroup-kill-saved-wconfig workgroup wconfig)
    (wg-add-to-wconfig-kill-ring wconfig)
    (wg-fontified-message
      (:cmd "Deleted: ")
      (:cur (wg-wconfig-name wconfig)))))


(defun wg-reverse-wconfig (wconfig &optional dir)
  "Reverse WCONFIG's wtree's wlist in direction DIR."
  (wg-asetf (wg-wconfig-wtree wconfig) (wg-reverse-wlist it dir))
  wconfig)


(provide 'workgroups-wconfig)
;;; workgroups-wconfig.el ends here
