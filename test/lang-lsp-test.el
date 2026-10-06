;;; lang-lsp-test.el --- Consistency checks for per-language LSP setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads every modules/lang-*.el in batch with `use-package' stubbed out, so only
;; top-level forms run, then checks the Eglot registrations they made.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/lang-lsp-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'eglot)
(require 'seq)
(require 'subr-x)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-stub-packages)

(defun lang-lsp-test--modes (entry)
  "Return the list of major modes named by the `eglot-server-programs' ENTRY."
  (let ((modes (car entry)))
    (delq nil (mapcar (lambda (m) (if (consp m) (car m) (and (symbolp m) m)))
                      (if (symbolp modes) (list modes) modes)))))

(defvar lang-lsp-test--entries nil
  "Server entries added by the lang-* modules (those not in Eglot's defaults).")

(let ((defaults (copy-sequence eglot-server-programs))
      (load-path (cons (expand-file-name "modules" test-helper-root) load-path)))
  (dolist (file (directory-files (expand-file-name "modules" test-helper-root)
                                 t "\\`lang-.*\\.el\\'"))
    (load file nil t))
  (setq lang-lsp-test--entries
        (seq-remove (lambda (e) (member e defaults)) eglot-server-programs)))

(ert-deftest lang-lsp-registers-servers ()
  (should (> (length lang-lsp-test--entries) 15)))

(defconst lang-lsp-test--not-autostarted
  '(heex-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode)
  "Modes named in a server entry that get no hook of their own.
heex-ts-mode only shares elixir-ls; the Clojure ones derive from
`clojure-ts-mode' and run its hook.")

(ert-deftest lang-lsp-installed-server-is-hooked ()
  "Every mode registered by a lang module autostarts when its binary exists."
  (dolist (entry lang-lsp-test--entries)
    (let ((cmd (cdr entry)))
      (when (and (consp cmd) (stringp (car cmd)) (executable-find (car cmd)))
        (dolist (mode (lang-lsp-test--modes entry))
          (unless (memq mode lang-lsp-test--not-autostarted)
            (let* ((hook (intern (format "%s-hook" mode)))
                   (fns (and (boundp hook) (symbol-value hook))))
              (should (or (memq #'eglot-ensure fns)
                          (memq #'ian/eglot-ensure-clojure fns))))))))))

(ert-deftest lang-lsp-no-module-requires-another ()
  "Load order in init.el is the only dependency between modules."
  (dolist (file (directory-files (expand-file-name "modules" test-helper-root)
                                 t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "^(require '\\(core\\|ui\\|tool\\|lang\\)-" nil t)))))

(provide 'lang-lsp-test)
;;; lang-lsp-test.el ends here
