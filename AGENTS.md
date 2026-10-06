# Repository Guidelines

## Project Structure & Module Organization
This repository is a modular Emacs configuration rooted at `~/.emacs.d`.

- `early-init.el`: startup and performance bootstrap.
- `init.el`: entry point that loads modules in order.
- `modules/`: primary source files (`core-*.el`, `ui-*.el`, `tool-*.el`, `lang-*.el`).
- `docs/KEYBINDINGS.org`: maintained keybinding reference.
- `custom/`: user Customize output.
- `private/`: local/private assets (avoid committing secrets).

Treat `straight/`, `eln-cache/`, `var/`, `desktop`, and `recentf` as runtime/package artifacts unless a change there is intentional.

## Build, Test, and Development Commands
No Makefile is used for the top-level config; use Emacs CLI checks.

- `emacs --debug-init`: full startup validation.
- `emacs --batch -Q --eval "(find-file \"modules/<file>.el\") (check-parens)"`: quick syntax/paren check.
- `rg "use-package <pkg>" modules/*.el`: verify package config is not duplicated.
- `rg "C-c <prefix>" modules/*.el`: check keybinding collisions before adding bindings.

First interactive launch (`emacs`) installs packages via `straight.el` when needed.

## Coding Style & Naming Conventions
- Use Emacs Lisp with file header `-*- lexical-binding: t; -*-`.
- Keep module names in `{category}-{name}.el` format.
- Prefer `use-package` blocks and lazy loading.
- Keep custom symbols prefixed with `ian/` (for functions/vars/customs).
- Preserve sectioned layout and existing commentary blocks.
- Use `C-c` prefixed keymaps for custom global bindings.

## Testing Guidelines
There is no dedicated automated test suite in this repo. Minimum validation for contributions:

1. Run `check-parens` on changed module files.
2. Run `emacs --debug-init` and confirm startup succeeds.
3. Check `*Warnings*` for new warnings after load.

## Commit & Pull Request Guidelines
Recent history favors short, imperative commit subjects (e.g., `Refactor configuration to modules`, `Fix ...`). Follow that style:

- Subject line in imperative mood, concise, and specific.
- Group related module edits in one commit; avoid mixing refactors and behavior changes.
- PRs should include: purpose, modules touched, manual validation steps run, and any user-visible keybinding/UI impacts.
