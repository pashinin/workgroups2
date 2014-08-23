;;; workgroups-advice --- change some functions behaviour
;;; Commentary:
;;; Code:

(require 'workgroups-utils-basic)
(require 'workgroups-session)
(require 'workgroups-association)


;; `wg-pre-window-configuration-change-hook' implementation advice
(cl-macrolet ((define-p-w-c-c-h-advice
             (fn)
             `(defadvice ,fn (before wg-pre-window-configuration-change-hook)
                "Call `wg-update-working-wconfig-hook' before this
function to save up-to-date undo information before the
window-configuration changes."
                (run-hooks 'wg-pre-window-configuration-change-hook))))
  (define-p-w-c-c-h-advice split-window)
  (define-p-w-c-c-h-advice enlarge-window)
  (define-p-w-c-c-h-advice delete-window)
  (define-p-w-c-c-h-advice delete-other-windows)
  (define-p-w-c-c-h-advice delete-windows-on)
  (define-p-w-c-c-h-advice switch-to-buffer)
  (define-p-w-c-c-h-advice set-window-buffer))


;; `save-buffers-kill-emacs' advice

(defadvice save-buffers-kill-emacs (around wg-freeze-wconfig)
  "`save-buffers-kill-emacs' calls `list-processes' when active
processes exist, screwing up the window config right before
Workgroups saves it.  This advice freezes `wg-current-wconfig' in
its correct state, prior to any window-config changes caused by
`s-b-k-e'."
  (wg-with-current-wconfig nil (wg-frame-to-wconfig)
    ad-do-it))


;; `select-frame' advice

(defadvice select-frame (before wg-update-current-workgroup-working-wconfig)
  "Update `selected-frame's current workgroup's working-wconfig.
Before selecting a new frame."
  (when wg-update-current-workgroup-working-wconfig-on-select-frame
    (wg-update-current-workgroup-working-wconfig)))


;; enable all advice

(defun wg-enable-all-advice ()
  "Enable and activate all of Workgroups' advice."
  ;; switch-to-buffer
  (ad-enable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-enable-advice 'switch-to-buffer 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'switch-to-buffer)

  ;; set-window-buffer
  (ad-enable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-enable-advice 'set-window-buffer 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'set-window-buffer)

  ;; split-window
  (ad-enable-advice 'split-window 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'split-window)

  ;; enlarge-window
  (ad-enable-advice 'enlarge-window 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'enlarge-window)

  ;; delete-window
  (ad-enable-advice 'delete-window 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'delete-window)

  ;; delete-other-windows
  (ad-enable-advice 'delete-other-windows 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'delete-other-windows)

  ;; delete-windows-on
  (ad-enable-advice 'delete-windows-on 'before 'wg-pre-window-configuration-change-hook)
  (ad-activate 'delete-windows-on)

  ;; save-buffers-kill-emacs
  (ad-enable-advice 'save-buffers-kill-emacs 'around 'wg-freeze-wconfig)
  (ad-activate 'save-buffers-kill-emacs)

  ;; select-frame
  ;;(ad-enable-advice 'select-frame 'before
  ;;                  'wg-update-current-workgroup-working-wconfig)
  ;;(ad-activate 'select-frame)
  )


;; disable all advice
;; (wg-disable-all-advice)
(defun wg-disable-all-advice ()
  "Disable and deactivate all of Workgroups' advice."
  ;; switch-to-buffer
  (ad-disable-advice 'switch-to-buffer 'after  'wg-auto-associate-buffer)
  (ad-disable-advice 'switch-to-buffer 'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'switch-to-buffer)

  ;; set-window-buffer
  (ad-disable-advice 'set-window-buffer 'after  'wg-auto-associate-buffer)
  (ad-disable-advice 'set-window-buffer 'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'set-window-buffer)

  ;; split-window
  (ad-disable-advice 'split-window 'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'split-window)

  ;; enlarge-window
  (ad-disable-advice 'enlarge-window 'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'enlarge-window)

  ;; delete-window
  (ad-disable-advice 'delete-window 'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'delete-window)

  ;; delete-other-windows
  (ad-disable-advice 'delete-other-windows 'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'delete-other-windows)

  ;; delete-windows-on
  (ad-disable-advice 'delete-windows-on    'before 'wg-pre-window-configuration-change-hook)
  (ad-deactivate 'delete-windows-on)

  ;; save-buffers-kill-emacs
  (ad-disable-advice 'save-buffers-kill-emacs 'around 'wg-freeze-wconfig)
  (ad-deactivate 'save-buffers-kill-emacs)

  ;; select-frame
  ;;(ad-disable-advice 'select-frame 'before
  ;;                   'wg-update-current-workgroup-working-wconfig)
  ;;(ad-deactivate 'select-frame)
  )


;; buffer auto-association advice

(defcustom wg-buffer-auto-association-on t
  "Non-nil means buffer auto-association is on.
-nil means it's off.  See `wg-buffer-auto-association'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-auto-association 'weak
  "Specifies the behavior for auto-associating buffers with workgroups.

When a buffer is made visible in a window it can be automatically
associated with the current workgroup in the window's frame.
This setting determines whether and how that happens.

Allowable values:

`weak' - weakly associate the buffer with the workgroup

`strong' - strongly associate the buffer with the workgroup

A function (a function-symbol or a lambda) - `funcall' the function to
determine whether and how to associate the buffer with the
workgroup.  The function should accept two arguments -- the
buffer and the workgroup -- and should return one of the
allowable values for this variable.

`nil' or any other value - don't associate the buffer with the
workgroup.

Becomes workgroup-local when set with `wg-set-workgroup-parameter'.
Becomes session-local when set with `wg-set-session-parameter'."
  :type 'sexp
  :group 'workgroups)

(defcustom wg-dissociate-buffer-on-kill-buffer t
  "Non-nil means dissociate from the current workgroup buffers
killed with `kill-buffer'."
  :type 'boolean
  :group 'workgroups)

(defun wg-auto-associate-buffer-helper (workgroup buffer assoc)
  "Associate BUFFER with WORKGROUP based on ASSOC.
See `wg-buffer-auto-association' for allowable values of ASSOC."
  (cond ((memq assoc '(weak strong))
         (wg-workgroup-associate-bufobj workgroup buffer (eq assoc 'weak)))
        ((functionp assoc)
         (wg-auto-associate-buffer-helper
          workgroup buffer (funcall assoc workgroup buffer)))
        (t nil)))

(defun wg-auto-associate-buffer (buffer &optional frame)
  "Conditionally associate BUFFER with the current workgroup in FRAME.
Frame defaults to `selected-frame'.  See `wg-buffer-auto-association'."
  (when wg-buffer-auto-association-on
    (-when-let* ((wg (wg-current-workgroup t frame))
                 (b (get-buffer buffer)))
      (unless (or (wg-workgroup-bufobj-association-type wg buffer)
                  (member wg wg-deactivation-list)
                  (member (buffer-name b) wg-associate-blacklist)
                  (not (or (buffer-file-name b)
                           (eq (buffer-local-value 'major-mode b) 'dired-mode))))
        (wg-auto-associate-buffer-helper
         wg buffer (wg-local-value 'wg-buffer-auto-association wg))))))

(defadvice switch-to-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (wg-auto-associate-buffer ad-return-value))

(defadvice set-window-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (wg-auto-associate-buffer
   (ad-get-arg 1)
   (window-frame (or (ad-get-arg 0) (selected-window)))))

(provide 'workgroups-advice)
;;; workgroups-advice.el ends here
