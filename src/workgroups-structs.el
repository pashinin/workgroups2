;;; workgroups-structs.el --- Define Elisp objects
;;; Commentary:
;;; Code:
(require 'workgroups-utils-basic)

(wg-defstruct wg session
  (uid (wg-generate-uid))
  (name)
  (modified)
  (parameters)
  (file-name)
  (version wg-version)
  (workgroup-list)
  (buf-list))

(wg-defstruct wg workgroup
  (uid (wg-generate-uid))
  (name)
  (modified)
  (parameters)
  (base-wconfig)
  (selected-frame-wconfig)
  (saved-wconfigs)
  (strong-buf-uids)
  (weak-buf-uids))

(wg-defstruct wg workgroup-state
  (undo-pointer)
  (undo-list))

(wg-defstruct wg wconfig
  (uid (wg-generate-uid))
  (name)
  (parameters)
  (left)
  (top)
  (width)
  (height)
  (vertical-scroll-bars)
  (scroll-bar-width)
  (wtree))

(wg-defstruct wg wtree
  (uid)
  (dir)
  (edges)
  (wlist))

(wg-defstruct wg win
  (uid)
  (parameters)
  (edges)
  (point)
  (start)
  (hscroll)
  (dedicated)
  (selected)
  (minibuffer-scroll)
  (buf-uid))

(wg-defstruct wg buf
  (uid (wg-generate-uid))
  (name)
  (file-name)
  (point)
  (mark)
  (local-vars)
  (special-data)
  ;; This may be used later:
  (gc))

(defmacro wg-workgroup-list ()
  "Setf'able `wg-current-session' modified slot accessor."
  `(wg-session-workgroup-list (wg-current-session)))

(provide 'workgroups-structs)
;;; workgroups-structs.el ends here
