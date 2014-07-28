;;; workgroups-modeline.el --- All modeline modifications
;;; Commentary:
;;; Code:

(require 'workgroups-keys)
(require 'workgroups-workgroup)

(defcustom wg-mode-line-display-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom wg-mode-line-use-faces nil
  "Non-nil means use faces in the mode-line display.
It can be tricky to choose faces that are visible in both active
and inactive mode-lines, so this feature defaults to off."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-disable (featurep 'powerline)
  "Do not do any modeline modifications.
There are problems with powerline."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-only-name t
  "Display only workgroup name in modeline without any flags."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-decor-left-brace "("
  "String displayed at the left of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-right-brace ")"
  "String displayed at the right of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-divider ":"
  "String displayed between elements of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-window-dedicated
  #("#" 0 1 (help-echo "This window is dedicated to its buffer."))
  "Indicates that the window is dedicated to its buffer."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-window-undedicated
  #("-" 0 1 (help-echo "This window is not dedicated to its buffer."))
  "Indicates that the window is not dedicated to its buffer."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-session-modified
  #("*" 0 1 (help-echo "The session is modified"))
  "Indicates that the session is modified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-session-unmodified
  #("-" 0 1 (help-echo "The session is unmodified"))
  "Indicates that the session is unmodified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-workgroup-modified
  #("*" 0 1 (help-echo "The current workgroup is modified"))
  "Indicates that the current workgroup is modified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-workgroup-unmodified
  #("-" 0 1 (help-echo "The current workgroup is unmodified"))
  "Indicates that the current workgroup is unmodified."
  :type 'string
  :group 'workgroups)



;;; mode-line

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((wg (wg-current-workgroup t))
        (wg-use-faces wg-mode-line-use-faces))
    (cond (wg (wg-fontify " "
                ;;(consp (cons :div wg-mode-line-decor-left-brace))
                ;;(keywordp (car (cons :div wg-mode-line-decor-left-brace)))
                ;;(:div wg-mode-line-decor-left-brace)
                (:brace wg-mode-line-decor-left-brace)
                (:mode (wg-workgroup-name wg))
                (if (not wg-mode-line-only-name)
                    (concat
                     (wg-add-face :div wg-mode-line-decor-divider)
                     (wg-add-face :div wg-mode-line-decor-divider)
                     (if (window-dedicated-p)
                         wg-mode-line-decor-window-dedicated
                       wg-mode-line-decor-window-undedicated)
                     (wg-add-face :div wg-mode-line-decor-divider)
                     (if (wg-session-modified (wg-current-session))
                         wg-mode-line-decor-session-modified
                       wg-mode-line-decor-session-unmodified)
                     (if (wg-workgroup-modified wg)
                         wg-mode-line-decor-workgroup-modified
                       wg-mode-line-decor-workgroup-unmodified)))
                (:brace wg-mode-line-decor-right-brace)))
          (t (if wg-display-nowg
                 (progn
                   (wg-fontify " "
                     (:brace wg-mode-line-decor-left-brace)
                     (:mode wg-nowg-string)
                     (:brace wg-mode-line-decor-right-brace)))
               "")))))

(defun wg-change-modeline ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (unless (or (assq 'wg-mode-line-display-on mode-line-format)
              wg-mode-line-disable)
    (let ((format '(wg-mode-line-display-on (:eval (wg-mode-line-string))))
          (pos (or (cl-position 'mode-line-position mode-line-format) 10)))
      (set-default 'mode-line-format
                   (wg-insert-after format mode-line-format pos))
      (force-mode-line-update))))


(defun wg-remove-mode-line-display ()
  "Remove Workgroups' mode-line format from `mode-line-format'."
  (wg-awhen (assq 'wg-mode-line-display-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))


(defun wg-toggle-mode-line-display ()
  "Toggle `wg-mode-line-display-on'."
  (interactive)
  (wg-toggle-and-message 'wg-mode-line-display-on))

(defun wg-add-workgroups-mode-minor-mode-entries ()
  "Add Workgroups' minor-mode entries.
Adds entries to `minor-mode-list', `minor-mode-alist' and
`minor-mode-map-alist'."
  (cl-pushnew 'workgroups-mode minor-mode-list)
  (cl-pushnew '(workgroups-mode wg-modeline-string) minor-mode-alist :test 'equal)
  (setq minor-mode-map-alist
        (cons (cons 'workgroups-mode (wg-make-workgroups-mode-map))
              (delete (assoc 'workgroups-mode minor-mode-map-alist)
                      minor-mode-map-alist))))


(provide 'workgroups-modeline)
;;; workgroups-modeline.el ends here
