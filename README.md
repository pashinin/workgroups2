# Workgroups for Emacs
[![Build Status](https://api.travis-ci.org/pashinin/workgroups2.png)](https://travis-ci.org/pashinin/workgroups2)

## What is it?

Workgroups is a session manager for Emacs.

- It saves all your opened buffers, their location and sizes on disk to restore later
- You can create several workspaces

You can also restore such buffers as: org-agenda, shell, magit-status, help.

Fork it and [add more special buffers support](https://github.com/pashinin/workgroups2/wiki/For-developers). Or even better - fix bugs.

## Install

Just install "workgroups2" from Melpa and activate it with `(workgroups-mode 1)` after everything else.

## Configure

    (require 'workgroups2)
    ;; if you start Emacs as "emacs --daemon"
    ;; turn of autoloading of workgroups:
    ;;(setq wg-use-default-session-file nil)

    (workgroups-mode 1)   ; put this one at the bottom of .emacs

You may want to configure it more:

    ;; Change prefix key (before activating WG)
    (setq wg-prefix-key (kbd "C-c z"))

    ;; Change workgroups session file
    (setq wg-default-session-file "~/.emacs.d/.emacs_workgroups"

    ;; Set your own keyboard shortcuts to reload/save/switch WG:
    (global-set-key (kbd "<pause>")     'wg-reload-session)
    (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
    (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
    (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

## Use

Most commands are bound to both `<prefix> <key>` and `<prefix> C-<key>`.

By default prefix is: "C-c z" (To change it - see settings above)

    <prefix> <key>
    <prefix> c    - create workgroup
    <prefix> A    - rename workgroup
    <prefix> k    - kill workgroup
    <prefix> v    - switch to workgroup
    <prefix> C-s  - save session
    <prefix> C-f  - load session

## Help

Type `<prefix> ?` (Eval `(wg-help)`) for more help.

This will bring up a help buffer listing all the commands and their bindings.

See the customization section in the source for details, or use:

    M-x customize-group RET workgroups RET


## Original Workgroups

There is a package on Melpa called "workgroups".
This extension is based on experimental branch of the [original repo](http://github.com/tlh/workgroups.el).

So great respect to the author. But it has not been updated for more
than 2 years and experimental branch was not released.

## License

Workgroups for Emacs is released under the GPL.
