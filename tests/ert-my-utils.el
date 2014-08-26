;;; ert-my-utils.el --- Changes to ERT tests
;;; Commentary:
;;
;; Hacks to output errors when testing GUI
;;
;;; Code:

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

This mainly sets up debugger-related bindings."
  (setf (ert--test-execution-info-next-debugger test-execution-info) debugger
        (ert--test-execution-info-ert-debug-on-error test-execution-info)
        ert-debug-on-error)
  (catch 'ert--pass
    ;; For now, each test gets its own temp buffer and its own
    ;; window excursion, just to be safe.  If this turns out to be
    ;; too expensive, we can remove it.
    ;;(with-temp-buffer
      ;;(save-window-excursion
      (let ((debugger (lambda (&rest args)
                        (ert--run-test-debugger test-execution-info
                                                args)))
            (debug-on-error t)
            (debug-on-quit t)
            ;; FIXME: Do we need to store the old binding of this
            ;; and consider it in `ert--run-test-debugger'?
            (debug-ignored-errors nil)
            (ert--infos '()))
        (funcall (ert-test-body (ert--test-execution-info-test
                                 test-execution-info))))
      ;;)
    ;;)
    (ert-pass))
  (setf (ert--test-execution-info-result test-execution-info)
        (make-ert-test-passed))
  nil)


(defun ert-run-tests-batch (&optional selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object."
  (unless selector (setq selector 't))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (cl-ecase event-type
       (run-started
        (cl-destructuring-bind (stats) event-args
          (message "Running %s tests (%s)"
                   (length (ert--stats-tests stats))
                   (ert--format-time-iso8601 (ert--stats-start-time stats)))))
       (run-ended
        (cl-destructuring-bind (stats abortedp) event-args
          (let ((unexpected (ert-stats-completed-unexpected stats))
                (skipped (ert-stats-skipped stats))
                (expected-failures (ert--stats-failed-expected stats)))
            (message "\n%sRan %s tests, %s results as expected%s%s (%s)%s\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ert-stats-total stats)
                     (ert-stats-completed-expected stats)
                     (if (zerop unexpected)
                         ""
                       (format ", %s unexpected" unexpected))
                     (if (zerop skipped)
                         ""
                       (format ", %s skipped" skipped))
                     (ert--format-time-iso8601 (ert--stats-end-time stats))
                     (if (zerop expected-failures)
                         ""
                       (format "\n%s expected failures" expected-failures)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (when (not (ert-test-result-expected-p test result))
                         (message "%9s  %S"
                                  (ert-string-for-test-result result nil)
                                  (ert-test-name test))))
              (message "%s" ""))
            (unless (zerop skipped)
              (message "%s skipped results:" skipped)
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (when (ert-test-result-type-p result :skipped)
                         (message "%9s  %S"
                                  (ert-string-for-test-result result nil)
                                  (ert-test-name test))))
              (message "%s" "")))))
       (test-started
        )
       (test-ended
        (cl-destructuring-bind (stats test result) event-args
          (unless (ert-test-result-expected-p test result)
            (cl-etypecase result
              (ert-test-passed
               (message "Test %S passed unexpectedly" (ert-test-name test)))
              (ert-test-result-with-condition
               (message "Test %S backtrace:" (ert-test-name test))
               (with-temp-buffer
                 (ert--print-backtrace (ert-test-result-with-condition-backtrace
                                        result))
                 (goto-char (point-min))
                 (while (not (eobp))
                   (let ((start (point))
                         (end (progn (end-of-line) (point))))
                     (setq end (min end
                                    (+ start ert-batch-backtrace-right-margin)))
                     (message "%s" (buffer-substring-no-properties
                                    start end)))
                   (forward-line 1)))
               (with-temp-buffer
                 (ert--insert-infos result)
                 (insert "    ")
                 (let ((print-escape-newlines t)
                       (print-level 5)
                       (print-length 10))
                   (ert--pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result)))
                 (goto-char (1- (point-max)))
                 (cl-assert (looking-at "\n"))
                 (delete-char 1)
                 (message "Test %S condition:" (ert-test-name test))
                 (message "%s" (buffer-string))))
              (ert-test-aborted-with-non-local-exit
               (message "Test %S aborted with non-local exit"
                        (ert-test-name test)))
              (ert-test-quit
               (message "Quit during %S" (ert-test-name test)))))
          (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                 (format-string (concat "%9s  %"
                                        (prin1-to-string (length max))
                                        "s/" max "  %S")))
            (message format-string
                     (ert-string-for-test-result result
                                                 (ert-test-result-expected-p
                                                  test result))
                     (1+ (ert--stats-test-pos stats test))
                     (ert-test-name test)))))))))


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

(provide 'ert-my-utils)
;;; ert-my-utils.el ends here
