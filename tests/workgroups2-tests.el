;;; workgroups2-tests.el --- Try something here
;;; Commentary:
;;; Code:

(require 'cl)
(require 'ert)
(require 'workgroups2)

(ert-deftest 000-initial ()
  (should (boundp workgroups-mode))
  (should-not workgroups-mode))

(ert-deftest 010-activate ()
  (setq wg-session-file "/tmp/wg-test")
  (workgroups-mode 1)
  (should workgroups-mode)
  (should (string= (wg-get-session-file) "/tmp/wg-test")))

(ert-deftest wg-modeline-string ()
  (should (string= (wg-mode-line-string) " (First workgroup:*-)"))
  (setq wg-mode-line-decor-left-brace "["
        wg-mode-line-decor-right-brace "]")
  (should (string= (wg-mode-line-string) " [First workgroup:*-]"))
  (setq wg-flag-modified nil)
  (should (string= (wg-mode-line-string) " [First workgroup]"))
  )

(ert-deftest wg-still-active ()
  (should workgroups-mode))

(provide 'workgroups2-tests)
;;; workgroups2-tests.el ends here
