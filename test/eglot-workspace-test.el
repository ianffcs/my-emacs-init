;;; eglot-workspace-test.el --- Eglot workspace config merges, never overwrites -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads tool-dev (the helper) and every lang-* module with `use-package'
;; stubbed, then checks each toolchain's workspace settings survived.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/eglot-workspace-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'eglot)
(require 'cl-lib)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-stub-packages)

(let ((load-path (cons (expand-file-name "modules" test-helper-root) load-path)))
  (setenv "FLUTTER_HOME" "/stub/flutter")
  (setq-default eglot-workspace-configuration nil)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (unless (equal cmd "agda-mode") "/stub"))))
    (load (expand-file-name "modules/tool-dev.el" test-helper-root) nil t)
    (dolist (file (directory-files (expand-file-name "modules" test-helper-root)
                                   t "\\`lang-.*\\.el\\'"))
      (load file nil t))))

(ert-deftest eglot-workspace-all-toolchain-keys-present ()
  (let ((cfg (default-value 'eglot-workspace-configuration)))
    (dolist (key '(:clojure-lsp :rust-analyzer :gopls :python.analysis :dart))
      (should (plist-member cfg key)))
    (should (equal (plist-get (plist-get cfg :dart) :flutterSdkPath) "/stub/flutter"))))

(ert-deftest eglot-workspace-helper-merges ()
  (let ((eglot-workspace-configuration '(:a 1)))
    (setq-default eglot-workspace-configuration '(:a 1))
    (ian/eglot-add-workspace-config :b 2)
    (should (equal (default-value 'eglot-workspace-configuration) '(:a 1 :b 2)))))

(provide 'eglot-workspace-test)
;;; eglot-workspace-test.el ends here
