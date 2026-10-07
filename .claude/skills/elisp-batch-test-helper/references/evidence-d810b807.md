**Files:**
- **`/home/ietcd/.emacs.d/test/test-helper.el` (new):** it provides `test-helper-root`, `test-helper-stub-packages` and `test-helper-load-module`, as designed.
  - `test-helper-stub-packages` uses `(defalias 'use-package (cons 'macro (lambda (&rest _) nil)))`. I checked it works: modules loaded under it still register their Eglot entries, which lang-lsp-test depends on.
  - `test-helper-load-module` binds `load-path` only for the duration of the load, as the old tests did, and returns the file.

**Pass counts (before and after are identical, 0 failures):**

| Test | Passed |
|---|---|
| lang-lsp-test | 3 |
| ui-theme-test | 3 |
| clipboard-test | 9 |
| org-paths-test | 6 |
| org-existing-paths-test | 1 |
| core-auth-test | 10 |
| chat-credentials-test (full init.el) | 4 |