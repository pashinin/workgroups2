;;; wg-commands-minibuffer --- minibuffer commands
;;; Commentary:
;;; Code:

(require 'workgroups-utils-basic)
(require 'workgroups-ido)
(require 'workgroups-buf)

(defvar wg-just-exited-minibuffer nil
  "Flag set by `minibuffer-exit-hook'.
To exempt from undoification those window-configuration changes
caused by exiting the minibuffer.  This is ugly, but necessary.
It may seem like we could just null out
`wg-undoify-window-configuration-change' in
`minibuffer-exit-hook', but that also prevents undoification of
window configuration changes triggered by commands called with
`execute-extended-command' -- i.e. it's just too coarse.")

(defcustom wg-no-confirm-on-destructive-operation nil
  "Do not request confirmation before various destructive operations.
Like `wg-reset'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-minibuffer-message-timeout 0.75
  "Bound to `minibuffer-message-timeout' when messaging while the
minibuffer is active."
  :type 'float
  :group 'workgroups)


;;
;; Functions
;;

(defun wg-read-object (prompt test warning &optional initial-contents keymap
                              read hist default-value inherit-input-method)
  "PROMPT for an object that satisfies TEST,
WARNING if necessary.  ARGS are `read-from-minibuffer's args,
after PROMPT."
  (cl-labels ((read () (read-from-minibuffer
                        prompt initial-contents keymap read hist
                        default-value inherit-input-method)))
    (let ((obj (read)))
      (when (and (equal obj "") default-value) (setq obj default-value))
      (while (not (funcall test obj))
        (message warning)
        (sit-for wg-minibuffer-message-timeout)
        (setq obj (read)))
      obj)))

(defun wg-minibuffer-inactive-p ()
  "Return t when `minibuffer-depth' is zero, nil otherwise."
  (zerop (minibuffer-depth)))


(defun wg-barf-on-active-minibuffer ()
  "Throw an error when the minibuffer is active."
  (when (not (wg-minibuffer-inactive-p))
    (error "Exit minibuffer to use workgroups functions!")))


(provide 'workgroups-minibuffer)
;;; workgroups-minibuffer.el ends here
