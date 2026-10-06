;;; core-auth-test.el --- Tests for shared credential lookup -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -l ert -l test/core-auth-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'auth-source)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'core-auth t)

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
      (should (equal (ian/ai-key 'anthropic) "auth-secret"))
      (should (= auth-source-search-call-count 2)))))

;; Note: `ian/authinfo-secret' stubs below ignore the user argument.
(ert-deftest core-auth-key-falls-back-to-environment-and-honors-noerror ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "OPENAI_API_KEY" "environment-secret")
    (setenv "GEMINI_API_KEY" nil)
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should (equal (ian/ai-key 'openai) "environment-secret"))
      (should-not (ian/ai-key 'google t))
      (should-error (ian/ai-key 'google)))))

(ert-deftest core-auth-key-resolves-host-aliases ()
  (let ((process-environment (copy-sequence process-environment)) (seen nil))
    (setenv "ANTHROPIC_API_KEY" nil)
    (cl-letf (((symbol-function 'ian/authinfo-secret)
               (lambda (host &optional _user)
                 (push host seen)
                 (and (equal host "api.claude.ai") "alias-secret"))))
      (should (equal "alias-secret" (ian/ai-key 'anthropic)))
      (should (equal '("api.claude.ai" "api.anthropic.com") seen)))))

(ert-deftest core-auth-unknown-provider-always-signals ()
  (should-error (ian/ai-key 'bogus))
  (should-error (ian/ai-key 'bogus t))
  (should-error (ian/ai-key-callback 'bogus))
  (should-error (ian/ai-key "api.openai.com" t)))

(ert-deftest core-auth-key-callback-resolves-at-request-time ()
  (let ((process-environment (copy-sequence process-environment))
        (callback (ian/ai-key-callback 'openai)))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (setenv "OPENAI_API_KEY" "first")
      (should (equal "first" (funcall callback)))
      (setenv "OPENAI_API_KEY" "second")
      (should (equal "second" (funcall callback))))))

(ert-deftest core-auth-available-provider-order ()
  (let ((keys nil))
    (cl-letf (((symbol-function 'ian/ai-key)
               (lambda (provider &optional noerror)
                 (or (cdr (assq provider keys))
                     (unless noerror (error "Missing synthetic key"))))))
      (should (eq 'openai-compatible (ian/ai-available-provider)))
      (setq keys '((anthropic . "k")))
      (should (eq 'claude (ian/ai-available-provider)))
      (setq keys '((anthropic . "k") (openai . "k")))
      (should (eq 'openai (ian/ai-available-provider))))))

(ert-deftest core-auth-includes-keepassxc-secret-service-source ()
  (should (member "secrets:Login" auth-sources)))

(provide 'core-auth-test)
;;; core-auth-test.el ends here
