;;; workgroups-specialbufs --- special buffers serialization
;;; Commentary:
;;
;; TODO:
;;  1. Add more special buffers support (that you use)
;;  2. Improve existing
;;
;;; Code:

(require 'dflet)
(require 'workgroups-variables)

(defmacro wg-support (mode pkg params)
  "Macro to create (de)serialization functions for a buffer.
You need to save/restore a specific MODE which is loaded from a
package PKG. In PARAMS you give local variables to save and a
deserialization function.
"
  `(let ((mode-str (symbol-name ,mode))
         (args ,params))

     (eval `(defun ,(intern (format "wg-deserialize-%s-buffer" mode-str)) (buffer)
        "DeSerialization function created with `wg-support'.
Gets saved variables and runs code to restore a BUFFER."
        (when (require ',,pkg nil 'noerror)
          (wg-dbind (this-function variables) (wg-buf-special-data buffer)
            (let ((default-directory (car variables))
                  (df (cdr (assoc 'deserialize ',,params)))
                  (user-vars (cdr variables)))
              (if df (funcall df buffer user-vars))
              (current-buffer)
            )))))

     (eval `(defun ,(intern (format "wg-serialize-%s-buffer" mode-str)) (buffer)
              "Serialization function created with `wg-support'.
Saves some variables to restore a BUFFER later."
              (when (get-buffer buffer)
                (with-current-buffer buffer
                  (when (eq major-mode ',,mode)
                    (let ((sf (cdr (assoc 'serialize ',,params))))
                      (list ',(intern (format "wg-deserialize-%s-buffer" mode-str))
                            (wg-take-until-unreadable (list default-directory
                                                            (if sf (funcall sf buffer))
                                                            )))))))))
     ;; Maybe change a docstring for functions
     ;;(put (intern (format "wg-serialize-%s-buffer" (symbol-name mode)))
     ;;     'function-documentation
     ;;     (format "A function created by `wg-support'."))

     ;; Add function to `wg-special-buffer-serdes-functions' variable
     (eval `(add-to-list 'wg-special-buffer-serdes-functions
                         ',(intern (format "wg-serialize-%s-buffer" mode-str)) t))
     ))


;; Dired

(defun wg-deserialize-dired-buffer (buf)
  "Deserialize Dired buffer."
  (wg-dbind (this-function params) (wg-buf-special-data buf)
    (let ((dir (car params)))
      (if (or wg-restore-remote-buffers
              (not (file-remote-p dir)))
          ;; TODO: try to restore parent dir if not exist
          (if (file-directory-p dir)
              (dired dir)))
      (current-buffer))))

(defun wg-serialize-dired-buffer (buffer)
  "Serialize Dired buffer."
  (with-current-buffer buffer
    (when (eq major-mode 'dired-mode)
      (list 'wg-deserialize-dired-buffer
            (wg-take-until-unreadable (list (or (buffer-file-name) default-directory)))
            ))))

;; Info buffer serdes

(defun wg-deserialize-Info-buffer (buf)
  "Deserialize an Info buffer."
  (require 'info)
  (wg-aif (cdr (wg-buf-special-data buf))
      (if (fboundp 'Info-find-node)
          (apply #'Info-find-node it))
    (info))
  (current-buffer))

(defun wg-serialize-Info-buffer (buffer)
  "Serialize an Info buffer."
  (with-current-buffer buffer
    (when (eq major-mode 'Info-mode)
      (wg-when-boundp (Info-current-file Info-current-node)
        (list 'wg-deserialize-Info-buffer
              Info-current-file
              Info-current-node)))))


;; help buffer serdes

(defun wg-deserialize-help-buffer (buf)
  "Deserialize a help buffer BUF.
See `wg-serialize-help-buffer'."
  (require 'help-mode)
  (wg-dbind (this-function item stack forward-stack) (wg-buf-special-data buf)
    (condition-case err
        (apply (car item) (cdr item))
      (error (message "%s" err)))
    (wg-awhen (get-buffer "*Help*")
      (set-buffer it)
      (wg-when-boundp (help-xref-stack help-xref-forward-stack)
        (setq help-xref-stack stack
              help-xref-forward-stack forward-stack))
      (current-buffer))))

(defun wg-serialize-help-buffer (buffer)
  "Serialize a help buffer BUFFER.
Since `help-mode' is used by many buffers that aren't actually
*Help* buffers (e.g. *Process List*), we also check that
`help-xref-stack-item' has a local binding."
  (with-current-buffer buffer
    (when (and (eq major-mode 'help-mode)
               (local-variable-p 'help-xref-stack-item)
               (boundp 'help-xref-stack-item)
               (boundp 'help-xref-stack)
               (boundp 'help-xref-forward-stack))
      (list 'wg-deserialize-help-buffer
            (wg-take-until-unreadable help-xref-stack-item)
            (mapcar 'wg-take-until-unreadable help-xref-stack)
            (mapcar 'wg-take-until-unreadable help-xref-forward-stack)))))


;; ielm buffer serdes

(defun wg-deserialize-ielm-buffer (buf)
  "Deserialize an `inferior-emacs-lisp-mode' buffer BUF."
  (ielm)
  (current-buffer))

(defun wg-serialize-ielm-buffer (buffer)
  "Serialize an `inferior-emacs-lisp-mode' buffer BUFFER."
  (with-current-buffer buffer
    (when (eq major-mode 'inferior-emacs-lisp-mode)
      (list 'wg-deserialize-ielm-buffer))))


;; Wanderlust modes:
;; WL - folders
(defun wg-deserialize-wl-folders-buffer (buf)
  ""
  (if (fboundp 'wl)
      (wg-dbind (this-function) (wg-buf-special-data buf)
        ;;(when (not (eq major-mode 'wl-folder-mode))
        (wl)
        (goto-char (point-max))
        (current-buffer)
        )))

(defun wg-serialize-wl-folders-buffer (buffer)
  ""
  (if (fboundp 'wl)
      (with-current-buffer buffer
        (when (eq major-mode 'wl-folder-mode)
          (list 'wg-deserialize-wl-folders-buffer
                )))))

;; WL - summary mode (list of mails)
;;(defun wg-deserialize-wl-summary-buffer (buf)
;;  ""
;;  (interactive)
;;  (if (fboundp 'wl)
;;      (wg-dbind (this-function param-list) (wg-buf-special-data buf)
;;        (when (not (eq major-mode 'wl-summary-mode))
;;          (let ((fld-name (car param-list)))
;;            ;;(switch-to-buffer "*scratch*")
;;            ;;(wl)
;;            ;;(wl-folder-jump-folder fld-name)
;;            ;;(message fld-name)
;;            ;;(goto-char (point-max))
;;            ;;(insert fld-name)
;;            (current-buffer)
;;          )))))
;;
;;(defun wg-serialize-wl-summary-buffer (buffer)
;;  ""
;;  (if (fboundp 'wl)
;;      (with-current-buffer buffer
;;        (when (eq major-mode 'wl-summary-mode)
;;          (list 'wg-deserialize-wl-summary-buffer
;;                (wg-take-until-unreadable (list wl-summary-buffer-folder-name))
;;                )))))
;;
;;
;;;; mime-view-mode
;;
;;(defun wg-deserialize-mime-view-buffer (buf)
;;  ""
;;  (wg-dbind (this-function) (wg-buf-special-data buf)
;;    (when (not (eq major-mode 'mime-view-mode))
;;      ;;(wl-summary-enter-handler 3570)     ; only in wl-summary-mode
;;      ;;(wl-summary-enter-handler)     ; only in wl-summary-mode
;;      (current-buffer)
;;      )))
;;
;;(defun wg-serialize-mime-view-buffer (buffer)
;;  ""
;;  (with-current-buffer buffer
;;    (when (eq major-mode 'mime-view-mode)
;;      (list 'wg-deserialize-mime-view-buffer
;;            ))))


;; Magit status
(wg-support 'magit-status-mode 'magit
            '((deserialize . (lambda (buffer vars)
                               (when (file-directory-p default-directory)
                                 (magit-status default-directory))))))

;; Shell
(wg-support 'shell-mode 'shell
            '((deserialize . (lambda (buffer vars)
                               (shell (wg-buf-name buffer))))))

;; org-agenda buffer

(defun wg-get-org-agenda-view-commands ()
  "Return commands to restore the state of Agenda buffer.
Can be restored using \"(eval commands)\"."
  (interactive)
  (when (boundp 'org-agenda-buffer-name)
    (if (get-buffer org-agenda-buffer-name)
        (with-current-buffer org-agenda-buffer-name
          (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
                 (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
            (if series-redo-cmd
                (get-text-property p 'org-series-redo-cmd)
              (get-text-property p 'org-redo-cmd)))))))

(defun wg-run-agenda-cmd (f)
  "Run commands F in Agenda buffer.
You can get these commands using `wg-get-org-agenda-view-commands'."
  (when (and (boundp 'org-agenda-buffer-name)
             (fboundp 'org-current-line)
             (fboundp 'org-goto-line))
    (if (get-buffer org-agenda-buffer-name)
        (save-window-excursion
          (with-current-buffer org-agenda-buffer-name
            (let* ((line (org-current-line)))
              (if f (eval f))
              (org-goto-line line)))))))

(wg-support 'org-agenda-mode 'org-agenda
            '((serialize . (lambda (buffer)
                             (wg-get-org-agenda-view-commands)))
              (deserialize . (lambda (buffer vars)
                               (org-agenda-list)
                               (wg-awhen (get-buffer org-agenda-buffer-name)
                                 (set-buffer it)
                                 (wg-run-agenda-cmd (nth 0 vars)))
                               ))))


;; eshell
(wg-support 'eshell-mode 'esh-mode
            '((deserialize . (lambda (buffer vars)
                               (prog1 (eshell t)
                                 (rename-buffer (wg-buf-name buffer) t))))))


;; term-mode
;;
;; This should work for `ansi-term's, too, as there doesn't seem to
;; be any difference between the two except how the name of the
;; buffer is generated.
;;
;; (dflet ((term-window-width () 80)
;; (window-height () 24))
(wg-support 'term-mode 'term
            '((serialize . (lambda (buffer)
                             (wg-last1 (process-command (get-buffer-process buffer)))))
              (deserialize . (lambda (buffer vars)
                               (prog1(term (nth 0 vars))
                                 (rename-buffer (wg-buf-name buffer) t))))))


;; inferior-python-mode   (python.el)

(defun wg-deserialize-python-shell-buffer (buf)
  "Deserialize a python-shell buffer BUF.
Run shell with a last working directory."
  (wg-dbind (this-function args) (wg-buf-special-data buf)
    (let ((default-directory (car args))
          (pythoncmd (nth 1 args))
          (pythonargs (nth 2 args)))
      (save-window-excursion
        (run-python (concat pythoncmd " " pythonargs)))
      (wg-awhen (get-buffer (process-buffer (python-shell-get-or-create-process)))
        (set-buffer it)
        (switch-to-buffer (process-buffer (python-shell-get-or-create-process)))
        (goto-char (point-max)))
      (current-buffer)
      )))

(defun wg-serialize-python-shell-buffer (buffer)
  "Serialize a python-shell buffer BUFFER.
Saves shell current directory, python command and arguments."
  (with-current-buffer buffer
    (when (and (eq major-mode 'inferior-python-mode)
               (boundp 'python-shell-interpreter)
               (boundp 'python-shell-interpreter-args))
      (list 'wg-deserialize-python-shell-buffer
            (wg-take-until-unreadable (list default-directory
                                            python-shell-interpreter
                                            python-shell-interpreter-args))
            ))))

;;
;; `inferior-sage-mode'
;;
(defun wg-deserialize-sage-shell-buffer (buf)
  "Deserialize `inferior-sage-mode' shell buffer BUF."
  ;;run-sage (&optional new cmd noshow)
  (wg-dbind (this-function args) (wg-buf-special-data buf)
    (let ((default-directory (car args)))
      (save-window-excursion
        (run-sage t sage-command t))
      (wg-awhen sage-buffer
        (set-buffer it)
        (switch-to-buffer sage-buffer)
        (goto-char (point-max)))
      (current-buffer)
      )))

(defun wg-serialize-sage-shell-buffer (buffer)
  "Serialize `inferior-sage-mode' shell BUFFER."
  (with-current-buffer buffer
    (when (and (eq major-mode 'inferior-sage-mode)
               (boundp 'sage-command))
      (list 'wg-deserialize-sage-shell-buffer
            (wg-take-until-unreadable (list default-directory))))))


;; inferior-ess-mode   (ess-inf.el)

(defun wg-deserialize-ess-shell-buffer (buf)
  "Deserialize ess-shell buffer BUF.
Run shell with a last working directory."
  (when (require 'ess nil 'noerror)
    (if (fboundp 'inferior-ess-mode)
        (wg-dbind (this-function args) (wg-buf-special-data buf)
          (let ((default-directory (car args))
                (cmdname (nth 1 args))
                (ess-ask-about-transfile nil)
                (ess-ask-for-ess-directory nil)
                (ess-history-file nil))
            (R)
            (current-buffer)
            )))))

(defun wg-serialize-ess-shell-buffer (buffer)
  "Serialize a ess-shell buffer BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'inferior-ess-mode)
        (when (and (eq major-mode 'inferior-ess-mode)
                   (boundp 'inferior-ess-program))
          (list 'wg-deserialize-ess-shell-buffer
                (wg-take-until-unreadable (list default-directory
                                                inferior-ess-program))
                )))))


;; prolog-inferior-mode

(defun wg-deserialize-prolog-shell-buffer (buf)
  "Deserialize prolog shell buffer BUF."
  (when (require 'prolog nil 'noerror)
    (if (fboundp 'prolog-inferior-mode)
        (wg-dbind (this-function args) (wg-buf-special-data buf)
          (let ((default-directory (car args)))
            (save-window-excursion
              (run-prolog nil))
            (switch-to-buffer "*prolog*")
            (goto-char (point-max))  ; Don't know why it's not working for me
            (current-buffer)
            )))))

(defun wg-serialize-prolog-shell-buffer (buffer)
  "Serialize a prolog shell buffer BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'prolog-inferior-mode)
        (when (eq major-mode 'prolog-inferior-mode)
          (list 'wg-deserialize-prolog-shell-buffer
                (wg-take-until-unreadable (list default-directory))
                )))))


;; emms-playlist-mode
;;
;; Help me on this one:
;; 1. How to start emms without any user interaction?
;;
;;(defun wg-deserialize-emms-buffer (buf)
;;  "Deserialize emms-playlist buffer BUF."
;;  (when (require 'emms-setup nil 'noerror)
;;    (require 'emms-player-mplayer)
;;    (emms-standard)
;;    (emms-default-players)
;;    (if (fboundp 'emms-playlist-mode)
;;        (wg-dbind (this-function args) (wg-buf-special-data buf)
;;          (let ((default-directory (car args)))
;;            (save-window-excursion
;;              ;;(emms)
;;              (if (or (null emms-playlist-buffer)
;;                      (not (buffer-live-p emms-playlist-buffer)))
;;                  ;;(call-interactively 'emms-add-file)
;;                  (emms-source-directory "/usr/data/disk_3/Music/SORT/")
;;                ))
;;            ;; (emms)
;;            ;;(with-current-buffer emms-playlist-buffer-name
;;            ;;(emms-source-playlist-directory-tree "/usr/data/disk_3/Music/SORT/")
;;            ;;(emms-source-directory "/usr/data/disk_3/Music/SORT")
;;            ;;(switch-to-buffer emms-playlist-buffer-name)
;;            (emms-playlist-mode-go)
;;            (current-buffer)
;;            )))))
;;
;;(defun wg-serialize-emms-buffer (buffer)
;;  "Serialize emms BUFFER."
;;  (with-current-buffer buffer
;;    (if (fboundp 'emms-playlist-mode)
;;        (when (eq major-mode 'emms-playlist-mode)
;;          (list 'wg-deserialize-emms-buffer
;;                (wg-take-until-unreadable (list default-directory))
;;                )))))


;; compilation-mode
;;
;; I think it's not a good idea to compile a program just to switch
;; workgroups. So just restoring a buffer name.
(defun wg-deserialize-compilation-buffer (buf)
  "Deserialize compilation-mode buffer BUF."
  (when (require 'compile nil 'noerror)
    (wg-dbind (this-function args) (wg-buf-special-data buf)
      (let ((default-directory (car args))
            (bufname (nth 1 args))
            (arguments (nth 2 args)))
        (save-window-excursion
          (get-buffer-create bufname))
        (with-current-buffer bufname
          (make-local-variable 'compilation-arguments)
          (setq compilation-arguments args))
        (switch-to-buffer bufname)
        (goto-char (point-max))
        (current-buffer)
        ))))

(defun wg-serialize-compilation-buffer (buffer)
  "Serialize compilation BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'compilation-mode)
        (when (and (eq major-mode 'compilation-mode)
                   (boundp 'compilation-arguments))
          (list 'wg-deserialize-compilation-buffer
                (wg-take-until-unreadable (list default-directory
                                                (buffer-name)
                                                compilation-arguments))
                )))))

;; grep-mode
;; see grep.el - `compilation-start' - it is just a compilation buffer
;; local variables:
;; `compilation-arguments' == (cmd mode nil nil)
(defun wg-deserialize-grep-buffer (buf)
  "Deserialize grep-buffer BUF."
  (when (require 'grep nil 'noerror)
    (wg-dbind (this-function args) (wg-buf-special-data buf)
      (let ((default-directory (car args))
            (arguments (nth 1 args)))
        ;; (compilation-start "cmd" 'grep-mode)
        (compilation-start (car arguments) (nth 1 arguments))
        ))))

(defun wg-serialize-grep-buffer (buffer)
  "Serialize grep BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'grep-mode)
        (when (and (eq major-mode 'grep-mode)
                   (boundp 'compilation-arguments))
          (list 'wg-deserialize-grep-buffer
                (wg-take-until-unreadable (list default-directory
                                                compilation-arguments))
                )))))


;; speedbar-mode
(defun wg-deserialize-speedbar-buffer (buf)
  "Deserialize speedbar-buffer BUF."
  (when (and (require 'speedbar nil 'noerror)
             (require 'dframe nil 'noerror))
    (wg-dbind (this-function args) (wg-buf-special-data buf)
      (let ((default-directory (car args))
            bufname)
        (if (boundp 'sr-speedbar-buffer-name)
            (setq bufname sr-speedbar-buffer-name)
          (setq bufname "*SPEEDBAR*"))
        (when (and (fboundp 'speedbar-mode)
                   (fboundp 'speedbar-reconfigure-keymaps)
                   (fboundp 'speedbar-update-contents)
                   (fboundp 'speedbar-set-timer))
          (with-no-warnings
            (setq speedbar-buffer (get-buffer-create bufname))
            (setq speedbar-frame (selected-frame)
                  dframe-attached-frame (selected-frame)
                  speedbar-select-frame-method 'attached
                  speedbar-verbosity-level 0
                  speedbar-last-selected-file nil)
            (set-buffer speedbar-buffer)
            (speedbar-mode)
            (speedbar-reconfigure-keymaps)
            (speedbar-update-contents)
            (speedbar-set-timer 1)
            (set-window-dedicated-p (get-buffer-window bufname) t)
            (switch-to-buffer bufname)))
        (current-buffer)
        ))))


(defun wg-serialize-speedbar-buffer (buffer)
  "Serialize speedbar BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'speedbar-mode)
        (when (eq major-mode 'speedbar-mode)
          (list 'wg-deserialize-speedbar-buffer
                (wg-take-until-unreadable (list default-directory))
                )))))


(defun wg-deserialize-slime-buffer (buf)
  "Deserialize `slime' buffer BUF."
  (when (require 'slime nil 'noerror)
    (wg-dbind (this-function args) (wg-buf-special-data buf)
      (let ((default-directory (car args))
            (arguments (nth 1 args)))
        (save-window-excursion
          (slime-start* arguments))
        (switch-to-buffer (process-buffer (slime-process)))
        (current-buffer)
        ))))

;; `comint-mode'  (general mode for all shells)
;;
;; It may have different shells. So we need to determine which shell is
;; now in `comint-mode' and how to restore it.
;;
;; Just executing `comint-exec' may be not enough because we can miss
;; some hooks or any other stuff that is executed when you run a
;; specific shell.
(defun wg-serialize-comint-buffer (buffer)
  "Serialize comint BUFFER."
  (with-current-buffer buffer
    (if (fboundp 'comint-mode)
        (when (eq major-mode 'comint-mode)
          ;; `slime-inferior-lisp-args' var is used when in `slime'
          (when (and (boundp 'slime-inferior-lisp-args)
                     slime-inferior-lisp-args)
            (list 'wg-deserialize-slime-buffer
                  (wg-take-until-unreadable (list default-directory
                                                  slime-inferior-lisp-args))
                ))))))

;; Functions to serialize deserialize inf-mongo buffer
;; `mongo-command' is the command used to start inferior
;; mongo
(defun wg-deserialize-inf-mongo-buffer (buffer)
  "Deserialize an inf-mongo BUFFER."
  (when (require 'inf-mongo nil 'noerror)
    (wg-dbind (this-function mongo-command) (wg-buf-special-data buffer)
      (save-window-excursion
        (inf-mongo mongo-command))
      (when (get-buffer "*mongo*")
        (switch-to-buffer "*mongo*")
        (goto-char (point-max)))
      (current-buffer))))

(defun wg-serialize-inf-mongo-buffer (buffer)
  "Serialize an inf-mongo BUFFER."
  (with-current-buffer buffer
    (when (eq major-mode 'inf-mongo-mode)
      (list 'wg-deserialize-inf-mongo-buffer inf-mongo-command))))

(defun wg-temporarily-rename-buffer-if-exists (buffer)
  "Rename BUFFER if it exists"
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (rename-buffer "*wg--temp-buffer*" t))))

;; Functions to serialize deserialize inferior sml buffer
;; `inf-sml-program' is the program run as inferior sml, is the
;; `inf-sml-args' are the extra parameters passed, `inf-sml-host'
;; is the host on which sml was running when serialized
(defun wg-deserialize-inf-sml-buffer (buffer)
  "Deserialize an inferior-sml BUFFER."
  (when (require 'sml-mode nil 'noerror)
    (wg-dbind (this-function
               buf-name
               inf-sml-program
               inf-sml-args
               inf-sml-host) (wg-buf-special-data buffer)

      (save-window-excursion
        ;; If a inf-sml buffer already exists rename it temporarily
        ;; otherwise `run-sml' will simply switch to the existing
        ;; buffer, however we want to create a separate buffer with
        ;; the serialized name
        (let* ((inf-sml-buffer-name (concat "*"
                                            (file-name-nondirectory inf-sml-program)
                                            "*"))
               (existing-sml-buf (wg-temporarily-rename-buffer-if-exists
                                  inf-sml-buffer-name)))

          (with-current-buffer (run-sml inf-sml-program
                                        inf-sml-args
                                        inf-sml-host)

            ;; Rename the buffer
            (rename-buffer buf-name t)

            ;; Now we can re-rename the previously renamed buffer
            (when existing-sml-buf
              (with-current-buffer existing-sml-buf
                (rename-buffer inf-sml-buffer-name t)))

            (goto-char (point-max))
            (current-buffer)))))))

(defun wg-serialize-inf-sml-buffer (buffer)
  "Serialize an inferior sml BUFFER."
  (with-current-buffer buffer
    (when (eq major-mode 'inferior-sml-mode)
      (list 'wg-deserialize-inf-sml-buffer
            ;; If user has renamed this buffer we will need to
            ;; restore it with same name
            (buffer-name)
            sml-program-name
            sml-default-arg
            sml-host-name))))

;; Functions to restore geiser repls
(defun wg-deserialize-inf-geiser-buffer (buffer)
  "Deserialize an geiser repl BUFFER."
  (when (require 'geiser nil 'noerror)
    (wg-dbind (this-function implementation) (wg-buf-special-data buffer)
      (save-window-excursion
        (run-geiser implementation)
        (goto-char (point-max))
        (current-buffer)))))

(defun wg-serialize-inf-geiser-buffer (buffer)
  "Serialize an geiser repl BUFFER."
  (with-current-buffer buffer
    (when (eq major-mode 'geiser-repl-mode)
      (list 'wg-deserialize-inf-geiser-buffer
            geiser-impl--implementation))))


;;; buffer-local variable serdes

(defun wg-serialize-buffer-mark-ring ()
  "Return a new list of the positions of the marks in `mark-ring'."
  (mapcar 'marker-position mark-ring))

(defun wg-deserialize-buffer-mark-ring (positions)
  "Set `mark-ring' to a new list of markers created from POSITIONS."
  (setq mark-ring
        (mapcar (lambda (pos) (set-marker (make-marker) pos))
                positions)))

(defun wg-deserialize-buffer-major-mode (major-mode-symbol)
  "Conditionally retore MAJOR-MODE-SYMBOL in `current-buffer'."
  (and (fboundp major-mode-symbol)
       (not (eq major-mode-symbol major-mode))
       (funcall major-mode-symbol)))

(defun wg-deserialize-buffer-local-variables (buf)
  "Restore BUF's buffer local variables in `current-buffer'."
  (loop for ((var . val) . rest) on (wg-buf-local-vars buf)
        do (wg-awhen (assq var wg-buffer-local-variables-alist)
             (wg-dbind (var ser des) it
               (if des (funcall des val)
                 (set var val))))))

(provide 'workgroups-specialbufs)
;;; workgroups-specialbufs.el ends here
