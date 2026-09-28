# Emacs Configuration

A modular Emacs configuration (`straight.el` + `use-package`) organised into `core-`, `ui-`, `tool-` and `lang-` modules. This file records the domain vocabulary used when discussing its architecture.

## Language

**Language toolchain**:
The modes (including tree-sitter twins), LSP server command, formatter and autostart hook that make one programming language usable. Each `lang-*` module registers the toolchains it owns, using plain Emacs forms next to that language's other configuration.
_Avoid_: language profile, language spec, language support

**Mode twin**:
A `-ts-mode` major mode paired with its classic `-mode` counterpart. Every registration lists both explicitly.
_Avoid_: ts variant, tree-sitter mode alias

## Relationships

- A **Language toolchain** has one or more **Mode twins**; its LSP server and formatter are optional.
- A **Language toolchain** is registered in exactly one `lang-*` module, never in `tool-dev.el`.

## Flagged ambiguities

- `core-settings` establishes `org-directory` before modules that derive paths from it load. `lang-org` configures Org behavior and paths within that root; it does not own or reset the shared root.
- Registration is deliberately not abstracted: no `core-lang.el`, no `ian/lang-*` function. Each toolchain uses Emacs's own `eglot-server-programs`, mode hooks, `apheleia-mode-alist` and `apheleia-formatters` directly, so nothing is hidden behind a definition.
- Autostart is `eglot-ensure` on each mode's hook. Languages that autostarted before this layout hook it unconditionally. Newer ones are gated at load time by `executable-find` on the server binary, so a server installed mid-session needs a restart and one that exists only inside a project's `envrc` environment is missed.
- Registrations must be top-level forms, or `with-eval-after-load` forms at top level, not inside `use-package :config`. The consistency test stubs `use-package` and loads every `lang-*` module in batch.
- LSP backend: Eglot by default. Lean uses `lsp-mode` and Java uses `eglot-java` on purpose, and stay in their own modules.
