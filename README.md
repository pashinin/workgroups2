# Workgroups for Emacs
## What is it?

Workgroups is a session manager for Emacs.

- It saves all your opened buffers, their location and sizes on disk to restore later.
- You also can create several workspaces.

Isn't it enough?

## Install

1. Just install "workgroups2" from Melpa.

OR

2. use git and put `workgroups2` somewhere on your Emacs load path

        cd ~/.emacs.d/extensions
        git clone git://github.com/pashinin/workgroups2.git

- Byte-compile it if you want. This isn't required, but it'll speed some
  things up:

        C-u 0 M-x byte-recompile-directory <RET> ~/.emacs.d/extensions/workgroups2/

## Configure

- Load a module (if you installed it not from Melpa):

        (add-to-list 'load-path "~/.emacs.d/extensions/workgroups2")
        (require 'workgroups2)

- and set some parameters:

        ;; Settings:
        (desktop-save-mode t)                ; save opened files
        (setq wg-prefix-key (kbd "C-c z")
              wg-use-default-session-file nil ; turn off for "emacs --daemon"
              wg-default-session-file "~/.emacs_files/workgroups"
              wg-use-faces nil
              wg-morph-on nil)               ; animation off

        ;; Keyboard shortcuts - load, save, switch
        (global-set-key (kbd "<pause>")     'wg-reload-session)
        (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
        (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
        (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

        (workgroups-mode 1)     ; Activate workgroups

## Use

Most commands are bound to both `<prefix> <key>` and `<prefix> C-<key>`.

    <prefix> <key>
    <prefix> c    - create workgroup
    <prefix> k    - kill workgroup
    <prefix> v    - switch to workgroup
    <prefix> C-z  - save session
    <prefix> C-f  - load session

## Help
----------------------

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
