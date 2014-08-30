;;; ert-my-utils.el --- Changes to ERT tests
;;; Commentary:
;;
;; Hacks to output errors when testing GUI
;;
;;; Code:

(require 'cl-lib)
(require 'f)
(require 'ert)

(defun wg-tests-log (&optional ok)
  "Try to log with status OK."
  (with-current-buffer "*Messages*"
    (let ((txt (buffer-substring (point-min) (point-max))))
      (if ok
          (f-write-text txt 'utf-8 "/tmp/wg-tests-ok.log")
        (f-write-text txt 'utf-8 "/tmp/wg-tests.log")))))

(defun ert--run-test-internal (test-execution-info)
  "Low-level function to run a test according to TEST-EXECUTION-INFO.

My version without `save-window-excursion'."
  (setf (ert--test-execution-info-next-debugger test-execution-info) debugger
        (ert--test-execution-info-ert-debug-on-error test-execution-info)
        ert-debug-on-error)
  (catch 'ert--pass
    ;;(with-temp-buffer
    ;;(save-window-excursion
    (let ((debugger (lambda (&rest args)
                      (ert--run-test-debugger test-execution-info args)))
          (debug-on-error t)
          (debug-on-quit t)
          (debug-ignored-errors nil)
          (ert--infos '()))
      (funcall (ert-test-body (ert--test-execution-info-test
                               test-execution-info))))
    ;;))
    (ert-pass))
  (setf (ert--test-execution-info-result test-execution-info)
        (make-ert-test-passed))
  nil)

(defun my-ert-run-tests ()
  "My variant of `ert-run-tests-batch-and-exit'.
To hack this:
http://stackoverflow.com/questions/25490989/how-should-i-run-emacs-ert-tests-when-i-need-gui-tests"
  (unwind-protect
      (let ((stats (ert-run-tests-batch)))
        (if (zerop (ert-stats-completed-unexpected stats))
            (wg-tests-log t)
          (wg-tests-log))
        (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 0))
        )
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace)
          ;;(wg-tests-log)
          )
      (kill-emacs 0)
      )))

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
     (should (eq major-mode ,mode))
     ))

(provide 'ert-my-utils)
;;; ert-my-utils.el ends here
