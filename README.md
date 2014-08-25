[![Build Status](https://travis-ci.org/pashinin/workgroups2.png?branch=master)](https://travis-ci.org/pashinin/workgroups2)
[![Gittip](http://img.shields.io/gittip/pashinin.png)](https://www.gittip.com/pashinin)

# Workgroups for Emacs

## What is it?

Workgroups is a session manager for Emacs.

- It saves all your opened buffers, their location and sizes on disk to restore later
- You can create several workspaces

You can also restore such buffers as: org-agenda, shell, magit-status, help.

Fork it, [add more special buffers support](https://github.com/pashinin/workgroups2/wiki/How-to-restore-a-specific-type-of-buffer). Or even better - fix bugs.

## Install

Just install "workgroups2" from Melpa and activate it with

    (require 'workgroups2)
    ;; Change some settings
    (workgroups-mode 1)        ; put this one at the bottom of .emacs

## Use

Most commands are bound to both `<prefix> <key>` and `<prefix> C-<key>`.

By default prefix is: "C-c z" (To change it - see settings below)

    <prefix> <key>
    <prefix> c    - create workgroup
    <prefix> A    - rename workgroup
    <prefix> k    - kill workgroup
    <prefix> v    - switch to workgroup
    <prefix> C-s  - save session
    <prefix> C-f  - load session

## Configure

If you want to change some settings - here is an example:

    (require 'workgroups2)
    ;; Your settings here

    ;; autoload/autosave:
    ;;(setq wg-session-load-on-start nil)

    ;; Change prefix key (before activating WG)
    (setq wg-prefix-key (kbd "C-c z"))

    ;; Change workgroups session file
    (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

    ;; Set your own keyboard shortcuts to reload/save/switch WG:
    (global-set-key (kbd "<pause>")     'wg-reload-session)
    (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
    (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
    (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

    (workgroups-mode 1)   ; put this one at the bottom of .emacs

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
