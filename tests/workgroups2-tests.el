;;; workgroups2-tests.el --- Try something here
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'f)
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
  (let (message-log-max)
    (workgroups-mode 1))
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

  ;; Rename
  (wg-rename-workgroup "asd")
  (should (string= (wg-mode-line-string) " [asd:**]"))
  (wg-rename-workgroup "First workgroup")
  (should (string= (wg-mode-line-string) " [First workgroup:**]"))

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

(ert-deftest 100-wg-save ()
  (should (= (length (frame-list)) 1))
  (let (message-log-max)
    (wg-save-session))
  (should-not (wg-session-modified (wg-current-session)))
  (unless (string-equal "initial_terminal" (terminal-name (selected-frame)))
    (unless (file-exists-p "/tmp/wg-test")
      (error "WG session file wasn't created"))))


(defmacro test-pickel (value)
  "Test `wg-pickel' `wg-unpickel' on VALUE."
  `(progn
     (wg-unpickel (wg-pickel ,value))))

(ert-deftest 110-wg-pickel ()
  (test-pickel 123)
  (test-pickel "str")
  (test-pickel 'symbol)
  (test-pickel (current-buffer))
  ;; (get-buffer org-agenda-buffer-name)
  (test-pickel (point-marker))
  (test-pickel (list 'describe-variable 'help-xref-stack-item (get-buffer "*Help*")))
  ;; TODO:
  ;;(test-pickel (current-window-configuration))
  )



;;
;; Special buffers
;;

(require 'python)
(ert-deftest 300-special-modes ()
  (wg-test-special 'dired-mode 'dired
    (dired "/tmp"))

  (wg-test-special 'Info-mode 'info
    (info))

  (wg-test-special 'help-mode 'help-mode
    (describe-variable 'help-xref-stack-item)
    (switch-to-buffer "*Help*"))

  (wg-test-special 'magit-status-mode 'magit
    (magit-status "."))

  (wg-test-special 'shell-mode 'shell
    (shell))

  (wg-test-special 'term-mode 'term
    (term "/bin/sh"))

  (unless (version< emacs-version "24")
    (wg-test-special 'inferior-python-mode 'python
      (run-python python-shell-interpreter)
      (switch-to-buffer (process-buffer (python-shell-get-or-create-process)))))

  ;; TODO: handle errors
  )

(ert-deftest 310-frames ()
  (should wg-control-frames)
  (make-frame)
  (make-frame)
  (should (wg-modified-p))
  (should (= (length (frame-list)) 3))
  (should workgroups-mode)
  (let (message-log-max)
    (wg-save-session))
  (should-not (wg-session-modified (wg-current-session)))
  (should (= (length (wg-session-parameter 'frame-list)) 2))
  (delete-other-frames)
  (should (= (length (frame-list)) 1))
  (wg-reload-session)
  ;;(should (= (length (wg-session-parameter 'frame-list)) 2))
  (should (= (length (frame-list)) 3))
  (delete-other-frames)
  (let (message-log-max)
    (wg-save-session)))

(provide 'workgroups2-tests)
;;; workgroups2-tests.el ends here
