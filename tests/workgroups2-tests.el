;;; workgroups2-tests.el --- Try something here
;;; Commentary:
;;; Code:

(require 'ert)
(require 'workgroups2)

(ert-deftest 000-initial ()
  (if (file-exists-p "/tmp/wg-test")
      (delete-file "/tmp/wg-test"))
  ;;(should-not (string-equal "initial_terminal" (terminal-name (selected-frame))))
  (should (boundp workgroups-mode))
  (should-not workgroups-mode)
  (should wg-session-load-on-start))

(ert-deftest 010-activate ()
  ;;(if (file-exists-p "/tmp/wg-test")
  ;;    (delete-file "/tmp/wg-test"))
  (setq wg-session-file "/tmp/wg-test")
  ;;(setq wg-session-load-on-start nil)
  (wg-reset-internal (wg-make-session))
  (wg-open-session)
  (wg-create-first-wg)
  (should (string= (wg-get-session-file) "/tmp/wg-test")))

(ert-deftest 050-modify ()
  (split-window-vertically)
  (switch-to-buffer "*Messages*")
  ;; Check 2 buffers
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (should (wg-session-modified (wg-get-current-session)))))

(ert-deftest 100-wg-save ()
  (should (= (length (frame-list)) 1))
  (let (message-log-max)
    (wg-save-session))
  (should-not (wg-session-modified (wg-get-current-session)))
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (unless (file-exists-p "/tmp/wg-test")
      (error "WG session file wasn't created"))))

(defmacro test-pickel (value)
  "Test `wg-pickel' `wg-unpickel' on VALUE."
  `(eq (wg-unpickel (wg-pickel ,value)) ,value))

(ert-deftest 110-wg-pickel ()
  (test-pickel 123)
  (test-pickel "str")
  (test-pickel 'symbol)
  (test-pickel (current-buffer))  ; #<buffer tests.el>
  (test-pickel (point-marker))    ; #<marker at 3427 in tests.el>
  (test-pickel (make-marker))     ; #<marker in no buffer>
  (test-pickel (list 'describe-variable 'help-xref-stack-item (get-buffer "*Help*"))))

(ert-run-tests-batch-and-exit)

(provide 'workgroups2-tests)
;;; workgroups2-tests.el ends here
