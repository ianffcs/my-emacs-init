;;; org-ai-commands-test.el --- Tests for generated org-ai commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Run:
;;   emacs --batch -Q -l ert -l test/org-ai-commands-test.el -f ert-run-tests-batch-and-exit
;; tool-chat.el may fail to load fully under -Q; the commands table and
;; generator are top-level forms near the start of the org-ai section.

;;; Code:

(require 'ert)
(require 'subr-x)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(condition-case err
    (test-helper-load-module 'tool-chat t)
  (error (message "tool-chat load stopped: %S" err)))

(defconst org-ai-commands-test--expected
  '((ian/org-ai-complete-block . "C-c M-b")
    (ian/org-ai-chat-block . "C-c M-c")
    (ian/org-ai-code-block . "C-c M-C")
    (ian/org-ai-summarize-buffer . "C-c M-S")
    (ian/org-ai-explain-code . "C-c M-e")
    (ian/org-ai-improve-text . "C-c M-i")
    (ian/org-ai-translate . "C-c M-t")))

(ert-deftest org-ai-commands-exist-and-interactive ()
  (dolist (e org-ai-commands-test--expected)
    (should (commandp (car e)))
    (should (stringp (documentation (car e))))))

(ert-deftest org-ai-commands-keys-in-table ()
  (dolist (e org-ai-commands-test--expected)
    (should (equal (cdr e) (nth 1 (assq (car e) ian/org-ai-commands))))))

(ert-deftest org-ai-chat-block-inserts-and-positions-point ()
  (with-temp-buffer
    (ian/org-ai-chat-block)
    (should (equal "#+begin_ai :chat t\n[ME]: \n#+end_ai" (buffer-string)))
    (should (looking-at "\n#\\+end_ai"))))

(provide 'org-ai-commands-test)
;;; org-ai-commands-test.el ends here
