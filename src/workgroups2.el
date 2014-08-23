;;; workgroups2.el --- New workspaces for Emacs
;;
;; Copyright (C) 2013-2014 Sergey Pashinin
;; Copyright (C) 2010-2011 tlh
;;
;; Author: Sergey Pashinin <sergey at pashinin dot com>
;; Keywords: session management window-configuration persistence
;; Homepage: https://github.com/pashinin/workgroups2
;; Package-Requires: ((cl-lib "0.4"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;;
;;; Commentary:
;;
;; Workgroups2 is an Emacs session manager. It is based on the
;; experimental branch of the original "workgroups" extension.
;;
;; If you find a bug - please post it here:
;; https://github.com/pashinin/workgroups2/issues
;;
;;
;; Install
;; ----------------------
;; See the README.md file at: https://github.com/pashinin/workgroups2
;; Add the lines below to your .emacs configuration.
;;
;; (require 'workgroups2)
;;
;; <settings here>
;;
;; (workgroups-mode 1)  ; put this one at the bottom of .emacs
;;
;;
;; Configure
;; ----------------------
;; ;; Change prefix key (before activating WG)
;; (setq wg-prefix-key (kbd "C-c z"))
;;
;; ;; Change workgroups session file
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups"
;;
;; ;; Set your own keyboard shortcuts to reload/save/switch WG:
;; (global-set-key (kbd "<pause>")     'wg-reload-session)
;; (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
;; (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
;; (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)
;;
;;
;; Use
;; ----------------------
;; Most commands start with prefix `wg-prefix-key'.
;; You can change it before activating workgroups.
;; By default prefix is: "C-c z"
;;
;; <prefix> <key>
;;
;; <prefix> c    - create workgroup
;; <prefix> A    - rename workgroup
;; <prefix> k    - kill workgroup
;; <prefix> v    - switch to workgroup
;; <prefix> C-s  - save session
;; <prefix> C-f  - load session
;;
;;
;; Help
;; ----------------------
;; Type "<prefix> ?" for more help
;;
;; See also: https://github.com/pashinin/workgroups2/wiki
;;
;;; Code:

(require 'workgroups-pickel)
(require 'workgroups-faces)
(require 'workgroups-advice)
(require 'workgroups-modeline)
(require 'workgroups-keys)
(require 'workgroups-session)
(require 'workgroups-association)


(define-minor-mode workgroups-everywhere
  "Use Workgroups' buffer list filters everywhere `read-buffer' is used."
  :global t
  :group 'workgroups
  (wg-awhen (get 'workgroups-everywhere 'read-buffer-fn)
    (when (eq read-buffer-function 'wg-read-buffer)
      (setq read-buffer-function it))
    (put 'workgroups-everywhere 'read-buffer-fn nil))
  (when workgroups-everywhere
    (put 'workgroups-everywhere 'read-buffer-fn read-buffer-function)
    (setq read-buffer-function 'wg-read-buffer)))


(defun wg-add-or-remove-workgroups-hooks (remove)
  "Add or remove all of Workgroups' hooks, depending on REMOVE."
  (wg-add-or-remove-hooks
   remove
   'kill-emacs-query-functions       'wg-save-session-on-emacs-exit
   'delete-frame-hook                'wg-update-working-wconfig-on-delete-frame
   'wg-pre-window-configuration-change-hook 'wg-update-working-wconfig-hook
   'window-configuration-change-hook 'wg-flag-window-configuration-changed
   'post-command-hook                'wg-undoify-window-configuration-change
   'minibuffer-exit-hook             'wg-flag-just-exited-minibuffer
   'kill-buffer-hook                 'wg-update-buffer-in-buf-list
   'kill-buffer-hook                 'wg-auto-dissociate-buffer-hook
   ;;'window-configuration-change-hook 'wg-associate-frame-buffers
   ))


(defun workgroups-mode (&optional arg)
  "Turn `workgroups-mode' on and off.
ARG is nil - toggle
ARG >= 1   - turn on
ARG == 0   - turn off
ARG is anything else, turn on `workgroups-mode'."
  (interactive (list current-prefix-arg))
  (setq workgroups-mode
        (cond ((not arg) (not workgroups-mode))
              ((integerp arg) (if (> arg 0) t nil))
              (t)))
  (let (delayed)
  (cond
   (workgroups-mode
    (if (boundp 'desktop-restore-frames)
        (setq desktop-restore-frames nil))
    (wg-reset-internal)
    (wg-add-workgroups-mode-minor-mode-entries)
    (wg-enable-all-advice)
    (wg-add-or-remove-workgroups-hooks nil)
    (wg-change-modeline)

    ;; Load session
    (when (and wg-session-load-on-start
               (file-exists-p wg-session-file))
      (condition-case err
          (wg-open-session wg-session-file)
        (error (message "Error finding `wg-session-file': %s" err))))
    (run-hooks 'workgroups-mode-hook))
   (t
    (wg-save-session-on-workgroups-mode-exit)
    (wg-disable-all-advice)
    (wg-add-or-remove-workgroups-hooks t)
    (wg-remove-mode-line-display)
    (run-hooks 'workgroups-mode-exit-hook)))
  (wg-fontified-message
    (:cmd "Workgroups Mode: ") (:msg (if workgroups-mode "on" "off")))
  (if (not delayed) (wg-create-first-wg))
  workgroups-mode))


(defun wg-help ()
  "Just call `apropos-command' on \"^wg-\".
There used to be a bunch of help-buffer construction stuff here,
including a `wg-help' variable that basically duplicated every
command's docstring;  But why, when there's `apropos-command'?"
  (interactive)
  (apropos-command "^wg-"))

(provide 'workgroups2)
;;; workgroups2.el ends here
