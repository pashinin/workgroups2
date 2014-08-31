[![License](https://img.shields.io/badge/license-GPL_3-green.svg?dummy)](https://github.com/pashinin/workgroups2)
[![Documentation](https://readthedocs.org/projects/workgroups2/badge/?version=latest)](http://workgroups2.readthedocs.org/en/latest/)
[![Build Status](https://travis-ci.org/pashinin/workgroups2.png?branch=master)](https://travis-ci.org/pashinin/workgroups2)
[![Gittip](http://img.shields.io/gittip/pashinin.png)](https://www.gittip.com/pashinin)

# Workgroups for Emacs

Workgroups is a session manager for Emacs.

- It saves all your opened buffers, their location and sizes on disk to restore later
- You can create several workspaces

You can also restore such buffers as: org-agenda, shell, magit-status, help.

Fork it, [add more special buffers support](https://github.com/pashinin/workgroups2/wiki/How-to-restore-a-specific-type-of-buffer). Or even better - fix bugs.

## Install

Just install "workgroups2" from Melpa and activate it with

```elisp
(require 'workgroups2)
;; Change some settings
(workgroups-mode 1)        ; put this one at the bottom of .emacs
```

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

```elisp
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
```
## More options

You can use `M-x customize-group` `workgroups` to see all variables and
faces to change.

```elisp
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
```

### Hooks

Hooks' names can tell when they are executed

```elisp
workgroups-mode-hook                    ; when `workgroups-mode' is turned on
workgroups-mode-exit-hook               ; `workgroups-mode' is turned off
wg-before-switch-to-workgroup-hook
wg-after-switch-to-workgroup-hook
```

## Help

For you:

1. `<prefix> ?` or `(wg-help)` - help buffer listing all the commands
   and their bindings.
2. `M-x customize-group RET workgroups RET` - all customization options

For me:

1. **Reporting a bug** - give me full `(backtrace)` of what
   happened and how you did it.
2. **Requesting a major-mode support** - write how you *install*,
   *configure* and *run* yours.
3. Make pull-requests
4. Ask questions (if you don't know how to hack it). Personally I
   found the code *very* complex at first


## License

GPL. This extension is based on experimental branch of the
[original repo](http://github.com/tlh/workgroups.el).
