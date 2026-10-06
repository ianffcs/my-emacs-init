;;; org-existing-paths-test.el --- Tests for ian/org-existing-paths -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads modules/core-settings.el and exercises the agenda-path filtering
;; helper directly.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/org-existing-paths-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(let ((user-emacs-directory test-helper-root)
      (load-path (cons (expand-file-name "modules" test-helper-root) load-path)))
  (load "core-settings" nil t))

(ert-deftest org-existing-paths-keeps-existing-drops-missing ()
  (let* ((dir (make-temp-file "org-paths-" t))
         (real (expand-file-name "real.org" dir))
         (missing (expand-file-name "missing.org" dir)))
    (with-temp-file real (insert "* test\n"))
    (should (equal (list real (expand-file-name dir))
                   (ian/org-existing-paths (list real missing dir))))
    (should (null (ian/org-existing-paths (list missing))))
    (should (null (ian/org-existing-paths nil)))))

(provide 'org-existing-paths-test)
;;; org-existing-paths-test.el ends here
