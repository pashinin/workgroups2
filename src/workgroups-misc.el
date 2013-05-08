;;; workgroups-misc --- functions just for help
;;; Commentary:
;;; Code:

(defun wg-string/starts-with (s arg)
  "Return non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))

(defun wg-is-file-remote (filename)
  "Return t if FILENAME starts with \"/ssh:\" or \"/sudo:\"."
  (interactive)
  (or (wg-string/starts-with filename "/ssh:")
      (wg-string/starts-with filename "/sudo:")))

;; (wg-is-file-remote "/sudo:/etc/myfile")

(provide 'workgroups-misc)
;;; workgroups-misc.el ends here
