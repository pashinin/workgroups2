;;; workgroups2-tests.el --- Try something here
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'workgroups2)

(ert-deftest 000-initial ()
  ;;(make-frame)
  (if (file-exists-p "/tmp/wg-tests.log")
      (delete-file "/tmp/wg-tests.log"))
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
  (wg-open-session wg-session-file)
  (should (string= (wg-get-session-file) "/tmp/wg-test")))

(ert-deftest 030-wg-utils ()
  (workgroups-mode 1)
  ;; (message "(wg-all-buf-uids)=%s" (wg-all-buf-uids))
  (should (= (length (wg-all-buf-uids)) 1))
  (should (wg-frame-to-wconfig))
  )

(ert-deftest 040-wg-still-active ()
  (should workgroups-mode))

(ert-deftest 050-modify ()
  (split-window-vertically)
  (switch-to-buffer "*Messages*")
  ;; Check 2 buffers
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (should (wg-session-modified (wg-current-session)))))

(ert-deftest 055-structs ()
  (let* ((s (wg-current-session))
         (wgs (wg-session-workgroup-list s))
         (wg1 (car wgs))
         (bufs (wg-session-buf-list s)))
    (should s)
    ;;(should (wg-session-modified s))
    (should wgs)
    (should (string= "First workgroup" (wg-workgroup-name wg1)))
    (should bufs)
    )
  ;;(should-not (wg-current-wconfig))

  ;; wtree
  (let ((wtree (wg-window-tree-to-wtree)))
    (should wtree)
    (should-not (boundp 'window-tree))
    ;;(should (string= (wg-wtree-dir wtree) "a"))
    ;;(should-not (wg-wtree-wlist wtree))
    )
  )

(ert-deftest 100-wg-save ()
  (should (= (length (frame-list)) 1))
  (let (message-log-max)
    (wg-save-session))
  (should-not (wg-session-modified (wg-current-session)))
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (unless (file-exists-p "/tmp/wg-test")
      (error "WG session file wasn't created"))))


;; Test serialization functions

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
  (test-pickel (list 'describe-variable 'help-xref-stack-item (get-buffer "*Help*")))
  ;;(test-pickel (window-tree))
  ;; TODO:
  ;;(test-pickel (current-window-configuration))
  )


;; Special buffers

(require 'python)
(ert-deftest 300-special-modes ()
  (wg-test-special 'dired-mode 'dired
    (dired "/tmp"))

  (wg-test-special 'Info-mode 'info
    (info))

  (wg-test-special 'help-mode 'help-mode
    (describe-variable 'help-xref-stack-item)
    (switch-to-buffer "*Help*"))

  (wg-test-special 'shell-mode 'shell
    (shell))

  (wg-test-special 'term-mode 'term
    (term "/bin/sh"))

  (wg-test-special 'inferior-python-mode
                   'python
                   (run-python python-shell-interpreter)
                   (switch-to-buffer (process-buffer (python-shell-get-or-create-process))))

  ;; TODO: handle errors
  )

;; Bugs

;; https://github.com/pashinin/workgroups2/issues/48 TODO

(provide 'workgroups2-tests)
;;; workgroups2-tests.el ends here
