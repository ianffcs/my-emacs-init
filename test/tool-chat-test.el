;;; tool-chat-test.el --- Tests for tool-chat helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -l ert -l test/tool-chat-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'core-auth t)
(condition-case err
    (test-helper-load-module 'tool-chat t)
  (error (message "tool-chat load stopped: %S" err)))

(ert-deftest tool-chat-parse-ollama-models ()
  (should (equal '("a:latest" "b:7b")
                 (ian/ollama-parse-models
                  "{\"models\":[{\"name\":\"a:latest\",\"size\":1},{\"name\":\"b:7b\"}]}")))
  (should-not (ian/ollama-parse-models "{\"models\":[]}"))
  (should-not (ian/ollama-parse-models "{}"))
  (should-not (ian/ollama-parse-models "not json"))
  (should-not (ian/ollama-parse-models "{\"models\":[{\"id\":1}]}")))

(ert-deftest tool-chat-fetch-models-without-endpoint-calls-back-nil ()
  (let ((ian/ollama-host--value "") (got 'unset)
        (process-environment (copy-sequence process-environment)))
    (setenv "IAN_OLLAMA_HOST" nil)
    (ian/ollama-fetch-models (lambda (models) (setq got models)))
    (should (null got))))

(ert-deftest tool-chat-key-status-reports-qwen-source ()
  (let (shown)
    (cl-letf (((symbol-function 'ian/ai-key)
               (lambda (provider &optional _noerror) (and (eq provider 'qwen) "x")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (ian/ai-key-status)
      (should (string-match-p "qwen: ok" shown))
      (should (string-match-p "openai: missing" shown)))))

(provide 'tool-chat-test)
;;; tool-chat-test.el ends here
