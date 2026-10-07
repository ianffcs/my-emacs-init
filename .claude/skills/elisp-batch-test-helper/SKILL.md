---
name: elisp-batch-test-helper
category: testing
description: Use when consolidating ERT test boilerplate across files.
---

When multiple ERT test files repeat the same setup boilerplate (loading modules, stubbing macros, managing load-path), extract that setup into a shared helper module. Each test file loads the helper and calls its setup function before running tests.

Benefits: eliminates duplication, centralizes test infrastructure, makes the setup maintainable as the test suite grows.

See `references/elisp-test-helper-example.md` for patterns like root detection via `load-file-name` and macro stubbing with `defalias`.