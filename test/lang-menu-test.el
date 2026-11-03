;;; lang-menu-test.el --- Per-language transient menus bind on real features -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads the lang modules with `use-package' stubbed and `transient'
;; faked, then provides each major-mode feature and asserts the menu
;; command is bound on every Mode twin's keymap.
;;
;; Loading the real major-mode packages is not done in -Q.  Instead the
;; FEATURES below are a known-good list, checked against the sources
;; (cc-mode defines `java-mode-map', python.el provides only `python',
;; scheme.el provides `scheme', elisp-mode.el provides `elisp-mode', etc.).
;; A with-eval-after-load on a feature not in this list never fires, so
;; the binding assertion fails.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/lang-menu-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defconst lang-menu-test--specs
  '((ian/jvm-menu "C-c C-m" lang-jvm
     (cc-mode java-mode-map) (java-ts-mode java-ts-mode-map)
     (kotlin-mode kotlin-mode-map) (kotlin-ts-mode kotlin-ts-mode-map)
     (scala-mode scala-mode-map) (scala-ts-mode scala-ts-mode-map))
    (ian/python-menu "C-c C-m" lang-python
     (python python-mode-map python-ts-mode-map))
    (ian/systems-menu "C-c C-m" lang-systems
     (rust-mode rust-mode-map) (rust-ts-mode rust-ts-mode-map)
     (go-mode go-mode-map) (go-ts-mode go-ts-mode-map))
    (ian/elixir-menu "C-c C-m" lang-beam
     (elixir-ts-mode elixir-ts-mode-map))
    ;; C-c RET is macroexpand / clj-refactor prefix in these modes.
    (ian/lisp-menu "C-c L" lang-lisp
     (clojure-ts-mode clojure-ts-mode-map) (clojure-mode clojure-mode-map)
     (elisp-mode emacs-lisp-mode-map) (lisp-mode lisp-mode-map)
     (scheme scheme-mode-map) (racket-mode racket-mode-map))
    ;; Org's own C-c C-m is `org-ctrl-c-ret'; reveal keeps its sub-prefix.
    (ian/reveal-menu "C-c r r" lang-org
     (org org-mode-map)))
  "Each entry: (COMMAND KEY MODULE (FEATURE MAP...)...).")

(defun lang-menu-test--load-all ()
  "Fake transient, then load every module named in the specs."
  (test-helper-stub-packages)
  (defalias 'transient-define-prefix
    (cons 'macro (lambda (name &rest _)
                   `(defun ,name () (interactive) nil))))
  (dolist (spec lang-menu-test--specs)
    (condition-case err (test-helper-load-module (nth 2 spec))
      (error (message "load %s: %S" (nth 2 spec) err))))
  (provide 'transient))

(ert-deftest lang-menu-bound-on-real-features ()
  (lang-menu-test--load-all)
  (dolist (spec lang-menu-test--specs)
    (pcase-let ((`(,cmd ,key ,_mod . ,entries) spec))
      (should (fboundp cmd))
      (dolist (entry entries)
        (dolist (map (cdr entry))
          (set map (make-sparse-keymap)))
        (provide (car entry))
        (dolist (map (cdr entry))
          (should (eq cmd (lookup-key (symbol-value map) (kbd key)))))))))

(provide 'lang-menu-test)
;;; lang-menu-test.el ends here
