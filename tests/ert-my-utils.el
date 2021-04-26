;;; ert-my-utils.el --- Changes to ERT tests
;;; Commentary:
;;
;; Hacks to output errors when testing GUI
;;
;;; Code:

(require 'cl-lib)
(require 'ert)

(defun my-ert-run-tests ()
  "My variant of `ert-run-tests-batch-and-exit'.
To hack this:
http://stackoverflow.com/questions/25490989/how-should-i-run-emacs-ert-tests-when-i-need-gui-tests"
  (unwind-protect
      (let ((stats (ert-run-tests-batch)))
        (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 0)))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 0))))

(defmacro wg-test-special (mode pkg &rest body)
  "Test restoring MODE from PKG.
Create needed buffer by executing BODY.
Then tests will follow to save it and restore."
  (declare (indent 2))
  `(let ((wg-log-level 0)
         message-log-max)
     ;; prepare
     (delete-other-windows)
     (switch-to-buffer wg-default-buffer)

     ;; create a buffer
     (require ,pkg)
     ,@body
     (should (eq major-mode ,mode))
     (wg-save-session)

     ;; save and restore
     (workgroups-mode 0)
     (switch-to-buffer wg-default-buffer)
     (workgroups-mode 1)
     (should (eq major-mode ,mode))))

(provide 'ert-my-utils)
;;; ert-my-utils.el ends here
