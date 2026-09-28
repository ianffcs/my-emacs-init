;;; core-auth-test.el --- Tests for shared credential lookup -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -l ert -l test/core-auth-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'auth-source)

(defconst core-auth-test--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro use-package (&rest _) nil)
(defun straight-use-package (&rest _) nil)

(let ((load-path (cons (expand-file-name "modules" core-auth-test--root) load-path)))
  (load (expand-file-name "modules/core-auth.el" core-auth-test--root) nil t))

(ert-deftest core-auth-secret-accepts-string-secrets ()
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) (list (list :secret "test-secret")))))
    (should (equal (ian/authinfo-secret "example.test") "test-secret"))))

(ert-deftest core-auth-secret-evaluates-lazy-secrets ()
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) (list (list :secret (lambda () "test-secret"))))))
    (should (equal (ian/authinfo-secret "example.test") "test-secret"))))

(ert-deftest core-auth-secret-returns-nil-when-missing ()
  (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
    (should-not (ian/authinfo-secret "example.test"))))

(ert-deftest core-auth-key-uses-auth-source-alias-before-environment ()
  (let ((auth-source-search-call-count 0)
        (process-environment (copy-sequence process-environment)))
    (setenv "ANTHROPIC_API_KEY" "environment-secret")
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (setq auth-source-search-call-count (1+ auth-source-search-call-count))
                 (if (equal (plist-get args :host) "api.claude.ai")
                     (list (list :secret (lambda () "auth-secret")))
                   nil))))
      (should (equal (ian/get-key "api.anthropic.com") "auth-secret"))
      (should (= auth-source-search-call-count 2)))))

(ert-deftest core-auth-key-falls-back-to-environment-and-honors-noerror ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "OPENAI_API_KEY" "environment-secret")
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should (equal (ian/get-key "api.openai.com") "environment-secret"))
      (should-not (ian/get-key "unknown.example" t))
      (should-error (ian/get-key "unknown.example")))))

(ert-deftest core-auth-includes-keepassxc-secret-service-source ()
  (should (member "secrets:Login" auth-sources)))

(provide 'core-auth-test)
;;; core-auth-test.el ends here
