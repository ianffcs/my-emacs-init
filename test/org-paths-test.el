;;; org-paths-test.el --- Org path initialization checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -l ert -l test/org-paths-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defconst org-paths-test--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(let ((user-emacs-directory org-paths-test--root)
      (load-path (cons (expand-file-name "modules" org-paths-test--root) load-path)))
  (load "core-settings" nil t))

(ert-deftest org-directory-is-established-by-core-settings ()
  (should (equal org-directory (expand-file-name "org" "~"))))

(ert-deftest core-settings-loads-before-org-path-consumers ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "init.el" org-paths-test--root))
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

(provide 'org-paths-test)
;;; org-paths-test.el ends here
