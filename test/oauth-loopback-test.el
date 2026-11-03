;;; oauth-loopback-test.el --- Tests for the OAuth loopback parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Run:
;;   emacs --batch -Q -l ert -l test/oauth-loopback-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'url-util)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'tool-comm t)

(ert-deftest oauth-loopback-parse-success ()
  (let ((r (ian/oauth-parse-callback-request
            "GET /?code=abc%2Bdef&state=xyz HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (should (equal "abc+def" (plist-get r :code)))
    (should (equal "xyz" (plist-get r :state)))
    (should-not (plist-get r :error))))

(ert-deftest oauth-loopback-parse-error-param ()
  (let ((r (ian/oauth-parse-callback-request
            "GET /?error=access_denied&state=s HTTP/1.1\r\n\r\n")))
    (should (equal "access_denied" (plist-get r :error)))
    (should-not (plist-get r :code))
    (should (equal "s" (plist-get r :state)))))

(ert-deftest oauth-loopback-parse-malformed ()
  (should-not (ian/oauth-parse-callback-request ""))
  (should-not (ian/oauth-parse-callback-request "POST /?code=a HTTP/1.1"))
  (should-not (ian/oauth-parse-callback-request "GET / HTTP/1.1\r\n\r\n"))
  (should-not (ian/oauth-parse-callback-request "garbage")))

(ert-deftest oauth-loopback-state-not-checked-by-parser ()
  "The parser reports STATE verbatim; it does not validate it."
  (let ((r (ian/oauth-parse-callback-request "GET /?code=c HTTP/1.1\r\n")))
    (should (equal "c" (plist-get r :code)))
    (should-not (plist-get r :state))))

(provide 'oauth-loopback-test)
;;; oauth-loopback-test.el ends here
