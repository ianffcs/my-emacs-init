# Elisp Batch Test Helper Pattern

## Basic Helper Structure

Create `test/test-helper.el` with three components:

### 1. Root Detection

Find the repository root from the helper's own location:

```elisp
(defconst test-helper-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root.")
```

The `or load-file-name buffer-file-name` trick works both in batch mode (load-file-name is set) and interactive editing (buffer-file-name is set).

### 2. Macro Stubs

When loading modules without their packages:

```elisp
(defun test-helper-stub-packages ()
  "Stub use-package and straight-use-package macros."
  (defalias 'use-package (cons 'macro (lambda (&rest _) nil)))
  (defalias 'straight-use-package (lambda (&rest _) nil)))
```

This allows modules to load and register (e.g., Eglot entries) without requiring packages to be installed.

### 3. Module Loader

```elisp
(defun test-helper-load-module (name &optional stub)
  "Load modules/NAME.el with modules/ on load-path.
  When STUB is non-nil, stub package macros first."
  (when stub (test-helper-stub-packages))
  (let ((load-path (cons (expand-file-name "modules" test-helper-root) load-path))
        (file (expand-file-name (format "modules/%s.el" name) test-helper-root)))
    (load file nil t)
    file))
```

## How Tests Use It

Each test file loads the helper and calls it:

```elisp
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(test-helper-load-module 'core-ui t)  ; load core-ui.el with stubbed packages

;; Now run your tests...
```

## Adapting to Your Structure

- If modules are in a different directory, adjust the `expand-file-name` calls in `test-helper-load-module`
- If you don't use `use-package`, adjust or remove the stubs
- The root-detection pattern works for any test structure