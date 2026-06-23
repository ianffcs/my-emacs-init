# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Overview

Modular Emacs configuration using `straight.el` + `use-package`. 33 modules across 4 categories in `modules/`.

## Architecture

```
init.el          → Entry point, loads modules in order
early-init.el    → GC tuning, native-comp, GUI suppression
modules/         → All configuration (33 .el files)
  core-*.el      → 9 files: packages, settings, OS, utils, editor, UI, completion, auth, session
  ui-*.el        → 4 files: navigation, windows, buffers, dashboard
  tool-*.el      → 7 files: dev/LSP/git, shells, dired, AI, comm, media, games
  lang-*.el      → 13 files: language support
docs/
  KEYBINDINGS.org → Full keybinding reference
```

**Loading pattern**: `init.el` uses `(require 'module-name)`. Each module ends with `(provide 'module-name)`.

## Key Modules

- `core-packages.el` — straight.el bootstrap, use-package, gcmh, treesit-auto
- `core-completion.el` — Vertico, Consult, Corfu, Embark, Cape, Orderless
- `core-ui.el` — Modus themes (auto light/dark), fonts, doom-modeline
- `tool-dev.el` — Magit, Eglot (LSP for 25+ langs), Projectile, apheleia, envrc
- `tool-ai.el` — GPTel, Minuet, Whisper, Aider, MCP servers, org-ai
- `lang-proof.el` — Agda, Idris 2, Lean 4, TLA+
- `lang-org.el` — Org-mode, Org-roam, Babel, Reveal.js, Denote

## Testing Changes

```bash
# Syntax check a module
emacs --batch -Q --eval "(find-file \"modules/file.el\") (check-parens)"

# Full startup test
emacs --debug-init
```

Check `*Warnings*` buffer after startup.

## Conventions

### File Naming
- `{category}-{feature}.el` — e.g. `lang-python.el`, `tool-shell.el`
- Categories: `core-`, `ui-`, `tool-`, `lang-`

### Variables and Functions
- Variables: `ian/{name}` — e.g. `ian/private-dir`
- Functions: `ian/{verb}-{noun}` — e.g. `ian/cleanup-buffer`

### Keybinding Prefixes
All custom bindings under `C-c`. Each prefix opens a transient menu:

| Prefix  | Domain      | Prefix  | Domain      |
|---------|-------------|---------|-------------|
| `C-c b` | Buffers     | `C-c q` | Quit/util   |
| `C-c d` | Dired       | `C-c s` | Search      |
| `C-c e` | Editor      | `C-c t` | Terminals   |
| `C-c f` | Files       | `C-c w` | Windows     |
| `C-c g` | AI / Git    | `C-c y` | Snippets    |
| `C-c j` | Jump/nav    | `C-c l` | LSP/Eglot   |
| `C-c n` | Org-roam    | `C-c p` | Projectile  |

### use-package Pattern
```elisp
(use-package package-name
  :straight t
  :defer t
  :hook (mode . function)
  :bind ("C-c x y" . command)
  :custom
  (variable value)
  :config
  (setup-code))
```

## Adding New Configuration

### New Language
1. Find or create `modules/lang-{name}.el`
2. Add `(require 'lang-{name})` to `init.el`
3. Add `eglot-ensure` hook + server program, formatter via `with-eval-after-load 'apheleia`

### Before Modifying
```bash
# Check for keybinding conflict
grep -r "C-c x" modules/

# Check if package already exists
grep -r "use-package package-name" modules/
```

## Important Directories

- `var/` — Runtime data (backups, caches) — managed by no-littering
- `custom/` — Customize output (gitignored)
- `private/` — Credentials and secrets (gitignored)
- `straight/` — Package cache (gitignored)

## Notes

- `straight-check-for-modifications` is set to `find-when-checking` — run `M-x straight-check-package` after editing a package's source
- Run `M-x straight-freeze-versions` after a clean session to pin all package versions to `straight/versions/default.el`
- Theme switches automatically between `modus-operandi` (day) and `modus-vivendi` (night) via `ian/auto-theme`
- See `docs/KEYBINDINGS.org` for the full keybinding reference
- See `README.md` for architecture overview and modification guide
