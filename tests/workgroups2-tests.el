;;; workgroups2-tests.el --- Try something here
;;; Commentary:
;;; Code:

(require 'cl)
(require 'ert)
(require 'f)
(load-file (concat (file-name-directory load-file-name) "ert-my-utils.el"))
(require 'workgroups2)

;;(defmacro w-all-buf-uids (value)
;;  "Test `wg-all-buf-uids'."
;;  (declare (indent 2))
;;  `(progn
;;     (defface ,face ,spec ,doc ,@args)))

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
  (workgroups-mode 1)
  (should workgroups-mode)
  (should (string= (wg-get-session-file) "/tmp/wg-test")))

(ert-deftest 020-modeline-string ()
  (should (string= (wg-mode-line-string) " (First workgroup:--)"))
  (setq wg-mode-line-decor-left-brace "["
        wg-mode-line-decor-right-brace "]")
  (should (string= (wg-mode-line-string) " [First workgroup:--]"))
  (setq wg-flag-modified nil)
  (should (string= (wg-mode-line-string) " [First workgroup]"))
  (setq wg-flag-modified t)
  )

(ert-deftest 030-wg-utils ()
  (should (= (length (wg-all-buf-uids)) 1))
  (should (wg-frame-to-wconfig))
  )

(ert-deftest 040-wg-still-active ()
  (should workgroups-mode))

(ert-deftest 050-modify ()
  (split-window-vertically)
  (switch-to-buffer "*Messages*")
  ;; Check 2 buffers
  ;;(should-not (window-tree))
  ;;(should-not (wg-window-tree-to-wtree))
  ;;(should (= (length (wg-all-buf-uids)) 2))
  ;;(should-not (wg-current-workgroup))
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (should (wg-session-modified (wg-current-session))))
  )

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

(ert-deftest yyy-wg-save ()
  ;;(should-not (string-equal "initial_terminal" (terminal-name (selected-frame))))
  ;;(should-not (selected-frame))


  ;;(should-not (window-tree))
  (should (= (length (frame-list)) 1))
  ;;(should (= (length (wg-all-buf-uids)) 2))
  ;; (length (wg-workgroup-list))
  ;; (wg-workgroup-name (wg-current-workgroup))
  (wg-save-session)
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (unless (file-exists-p "/tmp/wg-test")
      (error "WG session file wasn't created"))))

(provide 'workgroups2-tests)
;;; workgroups2-tests.el ends here
