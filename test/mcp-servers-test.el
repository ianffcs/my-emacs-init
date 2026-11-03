;;; mcp-servers-test.el --- Tests for the shared MCP server table -*- lexical-binding: t; -*-

;;; Commentary:
;; Run:
;;   emacs --batch -Q -l ert -l test/mcp-servers-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'tool-mcp t)

(defvar mcp-hub-servers)
(defvar agent-shell-mcp-servers)
(defvar mcp-filesystem-server-project-root)

(defmacro mcp-servers-test--with-table (rows &rest body)
  "Run BODY with `ian/mcp-servers' bound to ROWS and consumers isolated."
  (declare (indent 1))
  `(let ((ian/mcp-servers ,rows)
         (mcp-hub-servers nil)
         (agent-shell-mcp-servers nil)
         (mcp-filesystem-server-project-root '("/tmp/a" "/tmp/b")))
     ,@body))

(ert-deftest mcp-servers-same-names-in-both-consumers ()
  (mcp-servers-test--with-table ian/mcp-servers
    (let ((hub (mapcar #'car (ian/mcp-hub-server-defs)))
          (agent (mapcar (lambda (s) (alist-get 'name s))
                         (ian/mcp-agent-shell-server-defs))))
      (should hub)
      (should (equal hub agent))
      (should (equal hub (mapcar #'car ian/mcp-servers))))))

(ert-deftest mcp-servers-roots-preserved-for-both ()
  (mcp-servers-test--with-table ian/mcp-servers
    (let ((hub (cdr (assoc "filesystem" (ian/mcp-hub-server-defs))))
          (agent (cl-find "filesystem" (ian/mcp-agent-shell-server-defs)
                          :key (lambda (s) (alist-get 'name s)) :test #'equal)))
      (should (equal '("/tmp/a" "/tmp/b") (plist-get hub :roots)))
      (should (equal '("/tmp/a" "/tmp/b")
                     (last (alist-get 'args agent) 2)))
      (should (equal (plist-get hub :args)
                     (butlast (alist-get 'args agent) 2))))))

(ert-deftest mcp-servers-env-value-with-equals ()
  (should (equal '(("K" . "a=b=c") ("E" . ""))
                 (ian/mcp-env-alist '("K=a=b=c" "E="))))
  (mcp-servers-test--with-table '(("s" :command "x" :env ("K=a=b")))
    (should (equal '(:K "a=b")
                   (plist-get (cdr (car (ian/mcp-hub-server-defs))) :env)))
    (should (equal '(((name . "K") (value . "a=b")))
                   (alist-get 'env (car (ian/mcp-agent-shell-server-defs)))))))

(ert-deftest mcp-servers-clojure-row-flows-through-table ()
  (mcp-servers-test--with-table ian/mcp-servers
    (ian/mcp-set-server "clojure" :command "clojure" :args '("-X:mcp" "--port" "7888"))
    (should (assoc "clojure" ian/mcp-servers))
    (should (equal '("-X:mcp" "--port" "7888")
                   (plist-get (cdr (assoc "clojure" mcp-hub-servers)) :args)))
    (should (cl-find "clojure" agent-shell-mcp-servers
                     :key (lambda (s) (alist-get 'name s)) :test #'equal))
    ;; Updating replaces the row instead of duplicating it.
    (ian/mcp-set-server "clojure" :command "clojure" :args '("-X:mcp" "--port" "9999"))
    (should (= 1 (cl-count "clojure" ian/mcp-servers :key #'car :test #'equal)))
    (should (equal "9999" (car (last (plist-get (cdr (assoc "clojure" mcp-hub-servers)) :args)))))))

(provide 'mcp-servers-test)
;;; mcp-servers-test.el ends here
