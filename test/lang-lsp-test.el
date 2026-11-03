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
(require 'cl-lib)
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
  ;; Hooks gated on `executable-find' must register on every machine, so load
  ;; the modules as if every server binary were installed.
  (cl-letf (((symbol-function 'executable-find) (lambda (cmd &rest _)
                                                   ;; agda-mode is run, not just probed
                                                   (unless (equal cmd "agda-mode") "/stub"))))
   ;; tool-dev loads before lang-* and owns `ian/eglot-add-workspace-config'.
   (load (expand-file-name "modules/tool-dev.el" test-helper-root) nil t)
   (dolist (file (directory-files (expand-file-name "modules" test-helper-root)
                                 t "\\`lang-.*\\.el\\'"))
    (load file nil t))
   ;; Run `with-eval-after-load' \='apheleia forms against a fresh alist.
   (defvar apheleia-mode-alist nil)
   (defvar apheleia-formatters nil)
   (provide 'apheleia))
  (setq lang-lsp-test--entries
        (seq-remove (lambda (e) (member e defaults)) eglot-server-programs)))

(ert-deftest lang-lsp-registers-servers ()
  (should (> (length lang-lsp-test--entries) 15)))

(defconst lang-lsp-test--not-autostarted
  '(heex-ts-mode clojure-ts-clojurec-mode clojure-ts-clojurescript-mode)
  "Modes named in a server entry that get no hook of their own.
heex-ts-mode only shares elixir-ls; the Clojure ones derive from
`clojure-ts-mode' and run its hook.")

(defun lang-lsp-test--autostarts-p (mode)
  "Non-nil when MODE's hook runs an Eglot autostart function."
  (let ((fns (let ((hook (intern (format "%s-hook" mode))))
               (and (boundp hook) (symbol-value hook)))))
    (or (memq #'eglot-ensure fns) (memq #'ian/eglot-ensure-clojure fns))))

(defun lang-lsp-test--server-modes ()
  "All modes named by the lang-* server entries, minus documented exceptions."
  (seq-difference
   (seq-uniq (apply #'append (mapcar #'lang-lsp-test--modes lang-lsp-test--entries)))
   lang-lsp-test--not-autostarted))

(ert-deftest lang-lsp-registered-server-is-hooked ()
  "Every mode registered by a lang module autostarts, whatever is installed."
  (let ((missing (seq-remove #'lang-lsp-test--autostarts-p (lang-lsp-test--server-modes))))
    (should-not missing)))

(defun lang-lsp-test--twin (mode)
  "Return the -mode/-ts-mode counterpart of MODE."
  (let ((name (symbol-name mode)))
    (intern (if (string-suffix-p "-ts-mode" name)
                (concat (string-remove-suffix "-ts-mode" name) "-mode")
              (concat (string-remove-suffix "-mode" name) "-ts-mode")))))

(ert-deftest lang-lsp-mode-twins-agree ()
  "A -mode and its -ts-mode twin are both registered, or neither, and both hooked.
Registered means named by any Eglot server entry; hooked means the hook runs
`eglot-ensure'.  A mode with a hook but no server entry (or the reverse) is a
twin that was named in only one of the two places."
  (let* ((registered (seq-uniq (apply #'append (mapcar #'lang-lsp-test--modes
                                                       eglot-server-programs))))
         (bad nil))
    (dolist (mode (lang-lsp-test--server-modes))
      (let ((twin (lang-lsp-test--twin mode)))
        (when (and (memq twin registered) (not (memq twin lang-lsp-test--not-autostarted))
                   (not (lang-lsp-test--autostarts-p twin)))
          (push (list 'registered-not-hooked twin) bad))
        (when (and (lang-lsp-test--autostarts-p twin) (not (memq twin registered)))
          (push (list 'hooked-not-registered twin) bad))))
    (should-not bad)))

(ert-deftest lang-lsp-formatter-twins-agree ()
  "If a registered mode has an apheleia formatter, its registered twin has one too."
  (let* ((registered (lang-lsp-test--server-modes))
         (bad nil))
    (dolist (mode registered)
      (let ((twin (lang-lsp-test--twin mode)))
        (when (and (assq mode apheleia-mode-alist) (memq twin registered)
                   (not (assq twin apheleia-mode-alist)))
          (push (list 'no-formatter twin) bad))))
    (should-not bad)))

(ert-deftest lang-lsp-no-module-requires-another ()
  "Load order in init.el is the only dependency between modules."
  (dolist (file (directory-files (expand-file-name "modules" test-helper-root)
                                 t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "^(require '\\(core\\|ui\\|tool\\|lang\\)-" nil t)))))

(provide 'lang-lsp-test)
;;; lang-lsp-test.el ends here
