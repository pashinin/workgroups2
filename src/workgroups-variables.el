;;; workgroups-variables --- Workgroups vars and consts
;;; Commentary:
;;; Code:

(defconst wg-version "1.1.1" "Current version of Workgroups.")

;;; customization

(defgroup workgroups nil
  "Workgroups for Emacs -- Emacs session manager"
  :group 'convenience)

(defcustom workgroups-mode nil
  "Non-nil if Workgroups mode is enabled."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group 'workgroups
  :type 'boolean)

(defcustom wg-first-wg-name "First workgroup"
  "Title of the first workgroup created."
  :type 'string
  :group 'workgroups)

(defcustom wg-modeline-string " wg"
  "Appears in modeline."
  :type 'string
  :group 'workgroups)

(defcustom wg-load-last-workgroup t
  "Load last active (not first) workgroup from all your workgroups if it exists."
  :group 'workgroups
  :type 'boolean)

(defcustom wg-control-frames t
  "Save/restore frames."
  :group 'workgroups
  :type 'boolean)


;; hooks

(defcustom workgroups-mode-hook nil
  "Hook run when `workgroups-mode' is turned on."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-mode-exit-hook nil
  "Hook run when `workgroups-mode' is turned off."
  :type 'hook
  :group 'workgroups)

(defcustom wg-switch-to-workgroup-hook nil
  "Hook run by `wg-switch-to-workgroup'."
  :type 'hook
  :group 'workgroups)

(defcustom wg-pre-window-configuration-change-hook nil
  "Hook run before any function that triggers
`window-configuration-change-hook'."
  :type 'hook
  :group 'workgroups)


(defcustom wg-open-this-wg nil
  "Try to open this workgroup on start.
If nil - nothing happens."
  :type 'string
  :group 'workgroups)

(defcustom wg-switch-to-first-workgroup-on-find-session-file t
  "Non-nil means switch to the first workgroup in a session file
when it's found with `wg-find-session-file'."
  :type 'boolean
  :group 'workgroups)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME:
;;
;; Only set `wg-workgroup-base-wconfig' on `wg-write-session-file' or
;; `delete-frame' and only with the most recently changed working-wconfig.
;; Then, since it's not overwritten on every call to
;; `wg-workgroup-working-wconfig', its restoration can be retried after manually
;; recreating buffers that couldn't be restored.  So it takes over the
;; 'incorrect restoration' portion of the base wconfig's duty.  All that leaves
;; to base wconfigs is that they're a saved wconfig the user felt was important.
;; So why not allow more of of them?  A workgroup could stash an unlimited
;; number of wconfigs.
;;
;; TODO:
;;
;;   * Write new commands for restoring stashed wconfigs
;;
;;   * Add this message on improper restoration of `base-wconfig':
;;
;;       "Unable to restore 'buf1', 'buf2'... Hit C-whatever to retry after
;;        manually recreating these buffers."
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; workgroup restoration customization

;; TODO: possibly add `buffer-file-coding-system', `text-scale-mode-amount'
(defcustom wg-buffer-local-variables-alist
  `((major-mode nil wg-deserialize-buffer-major-mode)
    (mark-ring wg-serialize-buffer-mark-ring wg-deserialize-buffer-mark-ring)
    (left-fringe-width nil nil)
    (right-fringe-width nil nil)
    (fringes-outside-margins nil nil)
    (left-margin-width nil nil)
    (right-margin-width nil nil)
    (vertical-scroll-bar nil nil))
  "Alist mapping buffer-local variable symbols to serdes functions.

The `car' of each entry should be a buffer-local variable symbol.

The `cadr' of the entry should be either nil or a function of no
arguments.  If nil, the variable's value is used as-is, and
should have a readable printed representation.  If a function,
`funcall'ing it should yield a serialization of the value of the
variable.

The `caddr' of the entry should be either nil or a function of
one argument.  If nil, the serialized value from above is
assigned to the variable as-is.  It a function, `funcall'ing it
on the serialized value from above should do whatever is
necessary to properly restore the original value of the variable.
For example, in the case of `major-mode' it should funcall the
value (a major-mode function symbol) rather than just assigning
it to `major-mode'."
  :type 'alist
  :group 'workgroups)


(defcustom wg-nowg-string "No workgroups"
  "Display this string if there are no workgroups and
`wg-display-nowg' is t."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-nowg nil
  "Display something if there are no workgroups."
  :type 'boolean
  :group 'workgroups)

;; What to restore:

(defcustom wg-restore-remote-buffers t
  "Restore buffers that get \"t\" with `file-remote-p'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-frame-position t
  "Non-nil means restore frame position on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-scroll-bars t
  "Non-nil means restore scroll-bar settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-fringes t
  "Non-nil means restore fringe settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-margins t
  "Non-nil means restore margin settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point t
  "Non-nil means restore `point' on workgroup restore.
This is included mainly so point restoration can be suspended
during `wg-morph' -- you probably want this non-nil."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point-max t
  "Controls point restoration when point is at `point-max'.
If `point' is at `point-max' when a wconfig is created, put
`point' back at `point-max' when the wconfig is restored, even if
`point-max' has increased in the meantime.  This is useful in,
say, irc buffers where `point-max' is constantly increasing."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-mark t
  "Non-nil means restore mark data on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-window-dedicated-p t
  "Non-nil means restore `window-dedicated-p' on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remember-frame-for-each-wg nil
  "When switching workgroups - restore frame parameters for each workgroup.

When nil - save/restore frame parameters to/from the first workgroup."
  :type 'boolean
  :group 'workgroups)


(defcustom wg-wconfig-undo-list-max 20
  "Number of past window configs to retain for undo."
  :type 'integer
  :group 'workgroups)

(defcustom wg-wconfig-kill-ring-max 20
  "Maximum length of the `wg-wconfig-kill-ring'."
  :type 'integer
  :group 'workgroups)


;; buffer-list filtration customization

(defcustom wg-buffer-list-filtration-on t
  "Non-nil means Workgroups' buffer-list filtration feature is on.
Nil means ido and iswitchb behave normally.  See
`wg-buffer-list-filter-definitions' for more info."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-list-filter-definitions
  '((all "all" wg-buffer-list-filter-all)
    (fallback "fallback" nil))
  "List of buffer list filter definitions.
Each entry should be a list containing an identifier symbol, a
prompt string, and a function form that's funcall'd to produce
the filtered buffer-list.

The prompt string is displayed as part of the minibuffer prompt
when its filter is active.

The function form should be either a function-symbol or a lambda, and
should take two arguments: a workgroup and a list of live Emacs
buffers.  The function should return a new list of live buffers,
typically by filtering its second argument in some way.

Default buffer-list-filters include:

`all'           All buffer names

`fallback'      A special case used to fallback to the
                original (non-ido/iswitchb) Emacs command.
                `fallback' isn't actually a buffer-list-filter
                itself, but can be used in
                `wg-buffer-list-filter-order-alist' just the
                same.

Becomes workgroup-local when set with `wg-set-workgroup-parameter'.
Becomes session-local when set with `wg-set-session-parameter'."
  :type 'list
  :group 'workgroups)

(defcustom wg-buffer-list-filter-order-alist
  '((default all fallback))
  "Alist defining the order in which filtered buffer-lists are presented.

The car of each entry should be the symbol of the original Emacs
command (not the ido or iswitchb remappings) -- i.e. one of
`switch-to-buffer', `switch-to-buffer-other-window',
`switch-to-buffer-other-frame', `kill-buffer', `next-buffer',
`previous-buffer', `display-buffer', `insert-buffer',
`read-buffer', or the special symbol `default', which defines the
buffer-list-filter order for all commands not present in this
alist.

The cdr of each entry should be a list of buffer-list-filter
identifiers defining the order in which filtered buffer-lists are
presented for the command.  See
`wg-buffer-list-filter-definitions'.

Becomes workgroup-local when set with `wg-set-workgroup-parameter'.
Becomes session-local when set with `wg-set-session-parameter'."
  :type 'alist
  :group 'workgroups)

(defcustom wg-center-rotate-buffer-list-display nil
  "Non-nil means rotate the buffer list display so that the
current buffer is in the center of the list.  This can make it
easier to see the where `wg-previous-buffer' will take you, but
it doesn't look right if the buffer list display is long enough
to wrap in the miniwindow."
  :type 'boolean
  :group 'workgroups)


;;; vars

(defvar wg-workgroups-mode-minor-mode-map-entry nil
  "Workgroups' minor-mode-map entry.")

(defvar wg-wconfig-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-buffer-uid nil
  "Symbol for the current buffer's wg-buf's uid.
Every Workgroups buffer object (wg-buf) has a uid.  When
Workgroups creates or encounters an Emacs buffer object
corresponding to a wg-buf, it tags it with the wg-buf's uid to
unambiguously pair the two.")
(make-variable-buffer-local 'wg-buffer-uid)


;; file and modified flag vars

(defvar wg-flag-modified t
  "Dynamically bound to nil around destructive operations to
temporarily disable flagging `modified'.")


;; undo vars

(defvar wg-window-configuration-changed nil
  "Flag set by `window-configuration-change-hook'.")

(defvar wg-already-updated-working-wconfig nil
  "Flag set by `wg-update-working-wconfig-hook'.")

(defvar wg-undoify-window-configuration-change t
  "Flag unset when changes to the window config shouldn't cause
workgroups' undo info to be updated.")




;; buffer-list-filter vars

(defvar wg-current-workgroup nil
  "Bound to the current workgroup in `wg-with-buffer-list-filters'.")

;; (defvar wg-current-buffer-command nil
;;   "Bound to the current buffer command in `wg-with-buffer-list-filters'.")

(defvar wg-current-buffer-list-filter-id nil
  "Bound to the current buffer-list-filter symbol in `wg-with-buffer-list-filters'.")

(defvar wg-previous-minibuffer-contents nil
  "Holds the previous minibuffer contents for re-insertion when
the buffer-list-filter is cycled.")

(defvar wg-ido-method-translations
  `((switch-to-buffer              . selected-window)
    (switch-to-buffer-other-window . other-window)
    (switch-to-buffer-other-frame  . other-frame)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer commands to ido buffer methods.")

(defvar wg-buffer-internal-default-buffer nil
  "Bound to `wg-buffer-internal's optional DEFAULT argument for
use by buffer list filtration hooks.")


;; wconfig restoration

(defvar wg-window-min-width 2
  "Bound to `window-min-width' when restoring wtrees. ")

(defvar wg-window-min-height 1
  "Bound to `window-min-height' when restoring wtrees.")

(defvar wg-window-min-pad 2
  "Added to `wg-window-min-foo' to produce the actual minimum window size.")

(defvar wg-actual-min-width (+ wg-window-min-width wg-window-min-pad)
  "Actual minimum window width when creating windows.")

(defvar wg-actual-min-height (+ wg-window-min-height wg-window-min-pad)
  "Actual minimum window height when creating windows.")

(defvar wg-min-edges `(0 0 ,wg-actual-min-width ,wg-actual-min-height)
  "Smallest allowable edge list of windows created by Workgroups.")

(defvar wg-null-edges '(0 0 0 0)
  "Null edge list.")

(defvar wg-window-tree-selected-window nil
  "Used during wconfig restoration to hold the selected window.")

(defvar wg-update-current-workgroup-working-wconfig-on-select-frame t
  "Non-nil means update `selected-frame's current workgroup's
working wconfig before `select-frame' selects a new frame.
let-bind this to nil around forms in which you don't want this to
happen.")

;; Remove after some time
(defalias 'wg-switch-to-buffer 'switch-to-buffer)

(provide 'workgroups-variables)
;;; workgroups-variables.el ends here
