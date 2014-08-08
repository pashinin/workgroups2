;;; workgroups-association.el --- buffer for workgroup
;;; Commentary:
;;; Code:

(require 'workgroups-structs)
(require 'workgroups-variables)
(require 'workgroups-utils-basic)

(defvar wg-buffer-workgroup nil
  "Associating each buffer with the workgroup.
In which it most recently appeared.")
(make-variable-buffer-local 'wg-buffer-workgroup)

(defun wg-workgroup-associated-buf-uids (workgroup)
  "Return a new list containing all of WORKGROUP's associated buf uids."
  (append (wg-workgroup-strong-buf-uids workgroup)
          (wg-workgroup-weak-buf-uids workgroup)))

(defun wg-workgroup-associated-bufs (workgroup)
  "Return a new list containing all of WORKGROUP's associated bufs."
  (delete nil (mapcar 'wg-find-buf-by-uid
                      (wg-workgroup-associated-buf-uids workgroup))))

(defun wg-workgroup-associated-buffers (workgroup)
  "Return a new list containing all of WORKGROUP's associated buffer objects."
  (delete nil (mapcar 'wg-restore-buffer
                      (wg-workgroup-associated-bufs workgroup))))

(defun wg-workgroup-strongly-associate-bufobj (workgroup bufobj)
  "Strongly associate BUFOBJ with WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (remp (wg-removef-p uid (wg-workgroup-weak-buf-uids workgroup)
                             :test 'string=))
         (addp (wg-pushnew-p uid (wg-workgroup-strong-buf-uids workgroup)
                             :test 'string=)))
    (when (or remp addp)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-weakly-associate-bufobj (workgroup bufobj)
  "Weakly associate BUFOBJ with WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (remp (wg-removef-p uid (wg-workgroup-strong-buf-uids workgroup)
                             :test 'string=))
         (addp (wg-pushnew-p uid (wg-workgroup-weak-buf-uids workgroup)
                             :test 'string=)))
    (when (or remp addp)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-associate-bufobj (workgroup bufobj &optional weak)
  "Associate BUFOBJ with WORKGROUP.
WEAK non-nil means weakly associate it.  Otherwise strongly associate it."
  (if weak (wg-workgroup-weakly-associate-bufobj workgroup bufobj)
    (wg-workgroup-strongly-associate-bufobj workgroup bufobj)))

(defun wg-workgroup-dissociate-bufobj (workgroup bufobj)
  "Dissociate BUFOBJ from WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (rem1p (wg-removef-p uid (wg-workgroup-strong-buf-uids workgroup)
                              :test 'string=))
         (rem2p (wg-removef-p uid (wg-workgroup-weak-buf-uids workgroup)
                              :test 'string=)))
    (when (or rem1p rem2p)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-dissociate-weakly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all weakly associated buffers."
  (when (wg-workgroup-weak-buf-uids workgroup)
    (wg-flag-workgroup-modified workgroup)
    (setf (wg-workgroup-weak-buf-uids workgroup) nil)))

(defun wg-workgroup-dissociate-strongly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all strongly associated buffers."
  (when (wg-workgroup-strong-buf-uids workgroup)
    (wg-flag-workgroup-modified workgroup)
    (setf (wg-workgroup-strong-buf-uids workgroup) nil)))

(defun wg-workgroup-dissociate-all-buffers (workgroup)
  "Dissociate from WORKGROUP all its associated buffers."
  (wg-workgroup-dissociate-weakly-associated-buffers workgroup)
  (wg-workgroup-dissociate-strongly-associated-buffers workgroup))

(defun wg-auto-dissociate-buffer-hook ()
  "`kill-buffer-hook' that automatically dissociates buffers from workgroups."
  (when wg-dissociate-buffer-on-kill-buffer
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-dissociate-bufobj it (current-buffer)))))

(defun wg-associate-buffer-with-workgroup (&optional workgroup buffer weak)
  "Associate BUFFER with WORKGROUP.
WEAK non-nil means weakly associate BUFFER."
  (interactive (list nil nil current-prefix-arg))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (buffer (or buffer (current-buffer)))
         (bname (buffer-name buffer))
         (wgname (wg-workgroup-name workgroup)))
    (if (wg-workgroup-associate-bufobj workgroup buffer weak)
        (wg-message "%s-associated %S with %s"
                    (if weak "Weakly" "Strongly") bname wgname)
      (wg-message "%S is already associated with %s" bname wgname))))

(defun wg-associate-visible-buffers-with-workgroup (&optional workgroup weak)
  "Associate all buffers visible in `selected-frame' with WORKGROUP.
WEAK non-nil means weakly associate them.  Otherwise strongly
associate them."
  (interactive (list nil current-prefix-arg))
  (let ((workgroup (wg-get-workgroup workgroup))
        (buffers (mapcar 'window-buffer (window-list))))
    (dolist (buffer buffers)
      (wg-workgroup-associate-bufobj workgroup buffer weak))
    (wg-fontified-message
      (:cmd (format "%s associated: " (if weak "Weakly" "Strongly")))
      (wg-buffer-list-display buffers))))

(defun wg-dissociate-buffer-from-workgroup (&optional workgroup buffer)
  "Dissociate BUFFER from WORKGROUP."
  (interactive (list nil nil))
  (let ((workgroup (wg-get-workgroup workgroup))
        (buffer (or buffer (current-buffer))))
    (wg-message
     (if (wg-workgroup-dissociate-bufobj workgroup buffer)
         "Dissociated %S from %s" "%S isn't associated with %s")
     (wg-buffer-name buffer)
     (wg-workgroup-name workgroup))))

(defun wg-associate-buffers (workgroup window-or-emacs-window-tree)
  "Associate the buffers visible in window elements of
WINDOW-OR-EMACS-WINDOW-TREE with the given WORKGROUP.
WINDOW-OR-EMACS-WINDOW-TREE must be either a window or a tree of
the form produced by `(car (window-tree))'."
  (wg-aif (windowp window-or-emacs-window-tree)
      (with-current-buffer (window-buffer window-or-emacs-window-tree)
        (setq wg-buffer-workgroup workgroup))
    (dolist (w (cddr window-or-emacs-window-tree))
      (when w (wg-associate-buffers workgroup w)))))

(defun wg-workgroup-bufobj-association-type (workgroup bufobj)
  "Return BUFOBJ's association-type in WORKGROUP, or nil if unassociated."
  (let ((uid (wg-bufobj-uid-or-add bufobj)))
    (or (and (member uid (wg-workgroup-strong-buf-uids workgroup)) 'strong)
        (and (member uid (wg-workgroup-weak-buf-uids workgroup)) 'weak))))

(defun wg-associate-frame-buffers ()
  "Associate visible buffers with the current workgroup.
Unless it is currently being deactivated."
  (wg-awhen (wg-current-workgroup :noerror)
    (unless (member it wg-deactivation-list)
      (wg-associate-buffers it (car (window-tree))))))

;; (switch-to-buffer)   -> (read-buffer-to-switch "Asd")
;; -> internal-complete-buffer-except -> internal-complete-buffer
;; minibuffer-complete  minibuffer-completion-table
;; ido-switch-buffer  -> buffer-list
;;(global-set-key "\C-d" delete-char)
;; fdefinition
;; (buffer-list)
;; jit-lock-context-fontify
;; (wg-workgroup-weak-buf-uids (wg-current-workgroup))
;; (wg-workgroup-associated-buf-uids (wg-current-workgroup))
;; (wg-workgroup-associated-bufs (wg-current-workgroup))
;; (wg-workgroup-associated-buffers (wg-current-workgroup))
;; (wg-associate-frame-buffers)

;; (wg-workgroup-dissociate-all-buffers (wg-current-workgroup))
;;(fmakunbound 'buffer-list)
;;(define-key (current-global-map) [remap buffer-list] 'wg-buffer-list)
;; (wg-buffer-list)
;;(defun buffer-list (&optional frame)
;;  (list (current-buffer)))


(provide 'workgroups-association)
;;; workgroups-association.el ends here
