# Workgroups for Emacs
## What it does?

- It saves all your opened buffers, their location and sizes on disk to restore later.
- You also can create several workspaces.

Isn't it enough?

## Usage

### Getting Workgroups

See [github repo](http://github.com/tlh/workgroups.el).

- Put `workgroups` somewhere on your Emacs load path

        cd ~/.emacs.d/extensions
        git clone git://github.com/pashinin/workgroups2.git
        git pull

### Installing

- Byte-compile it if you want. This isn't required, but it'll speed some
  things up:

        C-u 0 M-x byte-recompile-directory <RET> ~/.emacs.d/extensions/workgroups2/

- Load a module:

        (add-to-list 'load-path "~/.emacs.d/extensions/workgroups2")
        (require 'workgroups2)

- and set some parameters:

        ;; Settings:
        (setq wg-prefix-key (kbd "C-c z")
              wg-no-confirm t
              wg-file "~/.emacs_files/workgroups"
              wg-use-faces nil
              wg-morph-on nil                ; animation off
              wg-switch-on-load t)           ; load first on start
        (workgroups-mode 1)                  ; Activate workgroups

        ;; Keyboard shortcuts
        ;; Load workgroups:
        (global-set-key (kbd "<pause>") (lambda () (interactive)
                                      (wg-find-session-file wg-file)))
        ;; Save workgroups:
        (global-set-key (kbd "C-S-<pause>") 'wg-save-session)    ; Ctrl-Shift-pause
        ;; Switch:
        (global-set-key (kbd "s-z")  'wg-switch-to-workgroup)
        (global-set-key (kbd "s-/")  'wg-switch-to-previous-workgroup)
        ;; s-z == super + z == Win + z

## Tutorial

Most commands are bound to both `<prefix> <key>` and `<prefix> C-<key>`.

### Help

To bring up a help buffer listing all the commands and their bindings, hit
`<prefix> ?` (`wg-help`).

- There are many other customization options.  See the customization section in
  the source for details, or use:

        M-x customize-group RET workgroups RET

### Workgroup Creation

To start off, add a few workgroups.  Hit `<prefix> c` to issue the command
`wg-create-workgroup`, give it a name, hit `RET`, and a new workgroup is
created.  Maybe split the screen a few times with `C-x 2` and `C-x 3`, and
switch to different buffers in some of the windows to make it unique.  Repeat
this process a few times to create some different workgroups.

Every workgroup must have a unique name.  You can rename workgroups after
they've been created with `<prefix> A` (`wg-rename-workgroup`).

### Saving and Loading

Saving and loading was the original motivation for writing Workgroups.  You can
save your workgroups to a file with `<prefix> C-s` (`wg-save-session`)
and you can load workgroups from a file with `(wg-find-session-file wg-file)`.

Once you have a file of saved workgroups, it's convenient to load it on
Emacs startup. To do so you can add a line like this to your`.emacs`:

    (wg-find-session-file "~/.emacs_files/workgroups")

### Killing and Yanking

You can kill workgroups with `<prefix> k` (`wg-kill-workgroup`).  Killing a
workgroup deletes it from the list of workgroups, and copies its working config
to the kill ring.  You can yank killed wconfigs into the current frame with
`<prefix> y` (`wg-yank-config`).  If the last command was `wg-yank-config`,
calling it again will yank the *next* wconfig in the kill ring, and so on, much
like Emacs' own kill ring.

You can save a wconfig to the kill ring without killing its workgroup with the
kill-ring-save commands.  `<prefix> M-w` (`wg-kill-ring-save-working-config`)
saves the working config to the kill ring, and `<prefix> M-W`
(`wg-kill-ring-save-base-config`) saves the base config to the kill ring.

`<prefix> M-k` (`wg-kill-workgroup-and-buffers`) kills a workgroup, and all the
buffers visible in it, and `<prefix> K` (`wg-delete-other-workgroups`) deletes
all but the current workgroup.


### Cloning

Cloning a workgroup creates a new workgroup under a different name with the a
copy of the current workgroup's base and working configs.  `<prefix> C`
(`wg-clone-workgroup`) will clone the current workgroup.


### Offsetting and Swapping

You can move a workgroup leftward or rightward in the workgroups list with
`<prefix> ,` (`wg-offset-left`) and `<prefix> .` (`wg-offset-right`)
respectively.  These commands work cyclically, so when you offset a workgroup
leftward or rightward when it's already on the far left or right of the list, it
will wrap around to the other side.

`<prefix> x` (`wg-swap-workgroups`) will swap the position in the workgroups
list of the previously selected workgroup with that of the current workgroup.


## Original Workgroups

There is a package on Melpa called "workgroups".
This extension is based on experimental branch of the original repo:

https://github.com/tlh/workgroups.el

So great respect to the author. But it has not been updated for more
than 2 years and experimental branch was not released.

## License

Workgroups for Emacs is released under the GPL.
