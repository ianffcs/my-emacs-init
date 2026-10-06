;;; test-helper.el --- Shared setup for batch tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Each test loads this with:
;;   (load (expand-file-name "test-helper"
;;                           (file-name-directory (or load-file-name buffer-file-name)))
;;         nil t)
;; then calls `test-helper-load-module'.

;;; Code:

(defconst test-helper-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root, derived from this file's location.")

(defun test-helper-stub-packages ()
  "Stub `use-package' and `straight-use-package' so modules load without packages."
  (defalias 'use-package (cons 'macro (lambda (&rest _) nil)))
  (defalias 'straight-use-package (lambda (&rest _) nil)))

(defun test-helper-load-module (name &optional stub)
  "Load modules/NAME.el with modules/ on `load-path'.
When STUB is non-nil, stub package macros first.  Return the loaded file."
  (when stub (test-helper-stub-packages))
  (let ((load-path (cons (expand-file-name "modules" test-helper-root) load-path))
        (file (expand-file-name (format "modules/%s.el" name) test-helper-root)))
    (load file nil t)
    file))

(provide 'test-helper)
;;; test-helper.el ends here
