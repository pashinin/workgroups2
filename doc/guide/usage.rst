=======
 Usage
=======

The whole config should look like this:

.. code-block:: cl

   (require 'workgroups2)
   ;; Change some settings
   (workgroups-mode 1)        ; put this one at the bottom of .emacs (init.el)

Now you activated ``workgroups-mode``.

Basic commands
==============

Most commands are bound to both :kbd:`<prefix> <key>` and :kbd:`<prefix> C-<key>`.

By default prefix is: :kbd:`C-c z` (To change it - see settings below)

.. code-block:: cl

  <prefix> <key>
  <prefix> c          ; create workgroup
  <prefix> A          ; rename workgroup
  <prefix> k          ; kill workgroup
  <prefix> v          ; switch to workgroup
  <prefix> C-s        ; save session
  <prefix> C-f        ; load session


Settings
========

.. code-block:: cl

   (require 'workgroups2)
   ;; Your settings here

   ;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

   ;; Change prefix key (before activating WG)
   (setq wg-prefix-key (kbd "C-c z"))

   ;; Change workgroups session file
   (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

   ;; Set your own keyboard shortcuts to reload/save/switch WGs:
   ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
   (global-set-key (kbd "<pause>")     'wg-reload-session)
   (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
   (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
   (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

   (workgroups-mode 1)   ; put this one at the bottom of .emacs


More settings
-------------

You can use :kbd:`M-x customize-group RET workgroups` to see all
variables and faces to change.

.. code-block:: cl

   ;; What to do on Emacs exit / workgroups-mode exit?
   (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
   (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

   ;; Mode Line changes
   ;; Display workgroups in Mode Line?
   (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
   (setq wg-flag-modified t)                 ; Display modified flags as well
   (setq wg-mode-line-decor-left-brace "["
         wg-mode-line-decor-right-brace "]"  ; how to surround it
         wg-mode-line-decor-divider ":")

Hooks
-----

Hooks' names can tell when they are executed

.. code-block:: cl

   workgroups-mode-hook                    ; when `workgroups-mode' is turned on
   workgroups-mode-exit-hook               ; `workgroups-mode' is turned off
   wg-before-switch-to-workgroup-hook
   wg-after-switch-to-workgroup-hook
