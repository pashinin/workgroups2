;;; workgroups-ido --- ido and iswitchb compatibility
;;; Commentary:
;;; Code:

(require 'workgroups-variables)
(require 'cl-lib)
(require 'ido)

(defcustom wg-ido-entry-buffer-replacement-regexp "^ .*Minibuf.*$"
  "Regexp matching the name of a buffer to replace `ido-entry-buffer'.
The regexp should match the name of a live buffer that will never
be a completion candidate under normal circumstances.  You
probably don't want to change this.  See
`wg-get-sneaky-ido-entry-buffer-replacement'."
  :type 'regexp
  :group 'workgroups)


(defun wg-read-buffer-mode ()
  "Return the buffer switching package (ido or iswitchb) to use, or nil."
  (if (eq wg-current-buffer-list-filter-id 'fallback) 'fallback
    (cl-case (let (workgroups-mode) (command-remapping 'switch-to-buffer))
      (ido-switch-buffer 'ido)
      (otherwise 'fallback))))

(defun wg-read-buffer-function (&optional mode)
  "Return MODE's or `wg-read-buffer-mode's `read-buffer' function."
  (cl-case (or mode (wg-read-buffer-mode))
    (ido 'ido-read-buffer)
    (fallback (lambda (prompt &optional default require-match)
                (let (read-buffer-function)
                  (read-buffer prompt default require-match))))))

(defun wg-completing-read
  (prompt choices &optional pred require-match initial-input history default)
  "Do a completing read.  The function called depends on what's on."
  (cl-ecase (wg-read-buffer-mode)
    (ido
     (ido-completing-read prompt choices pred require-match
                          initial-input history default))
    (fallback
     (completing-read prompt choices pred require-match
                      initial-input history default))))

(defun wg-get-sneaky-ido-entry-buffer-replacement (&optional regexp)
  "Return a live buffer to replace `ido-entry-buffer'.
This is a workaround for an ido misfeature.  IMHO, ido should
respect the value of `ido-temp-list' after
`ido-make-buffer-list-hook' has been run, since the user's
preference for the final value of `ido-temp-list', if any, has
been expressed in that hook.  But ido conditionally rotates the
first match to the end after the hook has been run, based on the
value of `ido-entry-buffer'.  So as a workaround, set
`ido-entry-buffer' to a buffer that will never be a completion
candidate under normal circumstances.  See
`wg-ido-entry-buffer-replacement-regexp'."
  (wg-get-first-buffer-matching-regexp
   (or regexp wg-ido-entry-buffer-replacement-regexp)))

(defun wg-adjust-buffer-list-default (buflist &optional default)
  "Adjust BUFLIST based on DEFAULT.
DEFAULT is the default completion candidate, and defaults to
`wg-buffer-internal-default-buffer'.  Non-nil, this gets placed
at the beginning of BUFLIST.  Otherwise rotate BUFLIST."
  (wg-aif (or default wg-buffer-internal-default-buffer)
      (wg-move-elt it buflist 0)
    (wg-rotate-list buflist)))

(defun wg-finalize-buffer-list (buflist)
  "Run `wg-buffer-list-finalization-hook' and return
`wg-temp-buffer-list'."
  (let ((wg-temp-buffer-list buflist))
    (run-hooks 'wg-buffer-list-finalization-hook)
    wg-temp-buffer-list))

(defun wg-set-buffer-list-symbol (symbol)
  "Set SYMBOL to the filtered buffer-list."
  (when (and wg-current-buffer-list-filter-id (boundp symbol))
    (set symbol
         (wg-finalize-buffer-list
          (wg-adjust-buffer-list-default
           (wg-filtered-buffer-list t))))))

(defun wg-set-ido-buffer-list ()
  "Set `ido-temp-list' with `wg-set-buffer-list-symbol'.
Added to `ido-make-buffer-list-hook'."
  (wg-set-buffer-list-symbol 'ido-temp-list)
  (wg-when-boundp (ido-entry-buffer)
    (setq ido-entry-buffer (wg-get-sneaky-ido-entry-buffer-replacement))))

(provide 'workgroups-ido)
;;; workgroups-ido.el ends here
