;;; workgroups.keys.el --- Set default workgroups keys
;;; Commentary:
;;
;;; Code:

(require 'workgroups-variables)
(require 'workgroups-utils-basic)

(defcustom wg-prefix-key (kbd "C-c z")
  "Workgroups' prefix key.
Setting this variable requires that `workgroups-mode' be turned
off and then on again to take effect."
  :type 'string
  :group 'workgroups)

(defvar workgroups-mode-map nil
  "Workgroups Mode's keymap.")

(defun wg-fill-keymap (keymap &rest binds)
  "Return KEYMAP after defining in it all keybindings in BINDS."
  (while binds
    (define-key keymap (car binds) (cadr binds))
    (setq binds (cddr binds)))
  keymap)

(defvar wg-prefixed-map
  (wg-fill-keymap
   (make-sparse-keymap)

   ;; workgroups
   (kbd "C-c")        'wg-create-workgroup
   (kbd "c")          'wg-create-workgroup
   (kbd "C")          'wg-clone-workgroup
   (kbd "A")          'wg-rename-workgroup
   (kbd "C-'")        'wg-switch-to-workgroup
   (kbd "'")          'wg-switch-to-workgroup
   (kbd "C-v")        'wg-switch-to-workgroup
   (kbd "v")          'wg-switch-to-workgroup

   ;; session
   (kbd "C-s")        'wg-save-session
   (kbd "C-w")        'wg-save-session-as
   (kbd "C-f")        'wg-open-session

   ;; killing and yanking
   (kbd "C-k")        'wg-kill-workgroup
   (kbd "k")          'wg-kill-workgroup
   (kbd "M-W")        'wg-kill-ring-save-base-wconfig
   (kbd "M-w")        'wg-kill-ring-save-working-wconfig
   (kbd "C-y")        'wg-yank-wconfig
   (kbd "y")          'wg-yank-wconfig
   (kbd "M-k")        'wg-kill-workgroup-and-buffers
   (kbd "K")          'wg-delete-other-workgroups


   ;; workgroup switching
   (kbd "M-v")        'wg-switch-to-workgroup-other-frame
   (kbd "C-j")        'wg-switch-to-workgroup-at-index
   (kbd "j")          'wg-switch-to-workgroup-at-index
   (kbd "0")          'wg-switch-to-workgroup-at-index-0
   (kbd "1")          'wg-switch-to-workgroup-at-index-1
   (kbd "2")          'wg-switch-to-workgroup-at-index-2
   (kbd "3")          'wg-switch-to-workgroup-at-index-3
   (kbd "4")          'wg-switch-to-workgroup-at-index-4
   (kbd "5")          'wg-switch-to-workgroup-at-index-5
   (kbd "6")          'wg-switch-to-workgroup-at-index-6
   (kbd "7")          'wg-switch-to-workgroup-at-index-7
   (kbd "8")          'wg-switch-to-workgroup-at-index-8
   (kbd "9")          'wg-switch-to-workgroup-at-index-9
   (kbd "C-p")        'wg-switch-to-workgroup-left
   (kbd "p")          'wg-switch-to-workgroup-left
   (kbd "C-n")        'wg-switch-to-workgroup-right
   (kbd "n")          'wg-switch-to-workgroup-right
   (kbd "C-a")        'wg-switch-to-previous-workgroup
   (kbd "a")          'wg-switch-to-previous-workgroup


   ;; updating and reverting
   ;; wconfig undo/redo
   (kbd "C-r")        'wg-revert-workgroup
   (kbd "r")          'wg-revert-workgroup
   (kbd "C-S-r")      'wg-revert-all-workgroups
   (kbd "R")          'wg-revert-all-workgroups
   (kbd "<left>")     'wg-undo-wconfig-change
   (kbd "<right>")    'wg-redo-wconfig-change
   (kbd "[")          'wg-undo-wconfig-change
   (kbd "]")          'wg-redo-wconfig-change
   (kbd "{")          'wg-undo-once-all-workgroups
   (kbd "}")          'wg-redo-once-all-workgroups


   ;; wconfig save/restore
   (kbd "C-d C-s")    'wg-save-wconfig
   (kbd "C-d C-'")    'wg-restore-saved-wconfig
   (kbd "C-d C-k")    'wg-kill-saved-wconfig


   ;; workgroup movement
   (kbd "C-x")        'wg-swap-workgroups
   (kbd "C-,")        'wg-offset-workgroup-left
   (kbd "C-.")        'wg-offset-workgroup-right


   ;; window moving and frame reversal
   (kbd "|")          'wg-reverse-frame-horizontally
   (kbd "\\")         'wg-reverse-frame-vertically
   (kbd "/")          'wg-reverse-frame-horizontally-and-vertically


   ;; toggling
   (kbd "C-t C-m")    'wg-toggle-mode-line-display
   (kbd "C-t C-d")    'wg-toggle-window-dedicated-p


   ;; misc
   (kbd "!")          'wg-reset
   (kbd "?")          'wg-help

   )
  "The keymap that sits on `wg-prefix-key'.")

(defun wg-make-workgroups-mode-map ()
  "Return Workgroups' minor-mode-map.
This map includes `wg-prefixed-map' on `wg-prefix-key', as well
as Workgroups' command remappings."
  (let ((map (make-sparse-keymap)))
    (define-key map wg-prefix-key
      wg-prefixed-map)
    (when (and (fboundp 'winner-undo)
               (fboundp 'winner-redo))
      (define-key map [remap winner-undo] 'wg-undo-wconfig-change)
      (define-key map [remap winner-redo] 'wg-redo-wconfig-change))
    (setq workgroups-mode-map map)))


(provide 'workgroups-keys)
;;; workgroups-keys.el ends here
