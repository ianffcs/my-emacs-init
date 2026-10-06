;;; org-paths-test.el --- Org path initialization checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -l ert -l test/org-paths-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(let ((user-emacs-directory test-helper-root)
      (load-path (cons (expand-file-name "modules" test-helper-root) load-path)))
  (load "core-settings" nil t))

(ert-deftest org-directory-is-established-by-core-settings ()
  (should (equal org-directory (expand-file-name "org" "~"))))

(ert-deftest core-settings-loads-before-org-path-consumers ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "init.el" test-helper-root))
    (let ((settings (progn (goto-char (point-min))
                           (search-forward "(require 'core-settings)")))
          (consumers '("(require 'core-editor)"
                       "(require 'ui-dashboard)"
                       "(require 'tool-dired)"
                       "(require 'tool-chat)"
                       "(require 'tool-mcp)"
                       "(require 'tool-comm)"
                       "(require 'lang-org)"
                       "(require 'lang-latex)")))
      (dolist (consumer consumers)
        (goto-char (point-min))
        (should (< settings (search-forward consumer)))))))

(ert-deftest org-file-resolves-under-org-directory ()
  (dolist (entry ian/org-files)
    (should (equal (ian/org-file (car entry))
                   (expand-file-name (cdr entry) org-directory))))
  (should (equal (ian/org-file 'inbox) (expand-file-name "inbox.org" org-directory))))

(ert-deftest org-file-follows-let-bound-org-directory ()
  (let ((org-directory "/tmp/other-org"))
    (should (equal (ian/org-file 'todo) "/tmp/other-org/todo.org"))))

(ert-deftest org-file-errors-on-unknown-name ()
  (should-error (ian/org-file 'no-such-file)))

(ert-deftest modules-do-not-hard-code-named-org-files ()
  (let ((names '("inbox.org" "todo.org" "notes.org" "diary.org" "elfeed.org"))
        (offenders nil))
    (dolist (file (directory-files (expand-file-name "modules" test-helper-root)
                                   t "\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "core-settings.el")
        (with-temp-buffer
          (insert-file-contents file)
          (dolist (name names)
            (goto-char (point-min))
            (when (search-forward name nil t)
              (push (format "%s: %s" (file-name-nondirectory file) name) offenders))))))
    (should (null offenders))))

(provide 'org-paths-test)
;;; org-paths-test.el ends here
