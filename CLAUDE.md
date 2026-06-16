# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a modular Emacs configuration using `straight.el` + `use-package` for package management. Configuration is split into 32 modules across 4 categories in the `modules/` directory.

## Architecture

```
init.el          → Main entry point, loads modules in order
early-init.el    → Pre-init performance optimizations
modules/         → All configuration (32 .el files)
  core-*.el      → Package management, settings, completion, UI basics
  ui-*.el        → Navigation, windows, buffers, dashboard
  tool-*.el      → Git, LSP, shells, AI, communication
  lang-*.el      → Language-specific support (30+ languages)
```

**Loading pattern**: `init.el` uses `(require 'module-name)` to load modules. Each module ends with `(provide 'module-name)`.

## Key Modules

- `core-packages.el` - straight.el bootstrap, use-package setup
- `core-completion.el` - Vertico, Consult, Corfu, Embark stack
- `tool-dev.el` - Magit, Eglot (LSP), Projectile, TRAMP
- `tool-ai.el` - GPTel, Whisper, Aider integrations
- `lang-org.el` - Org-mode config (largest module, 847 lines)

## Commands

### Testing Changes

```bash
# Syntax check a module
emacs --batch -Q --eval "(find-file \"modules/file.el\") (check-parens)"

# Full startup test with debug
emacs --debug-init

# Check *Warnings* buffer after startup for issues
```

### First Launch

On first launch, `straight.el` automatically downloads and installs all packages. Be patient.

## Conventions

### File Naming
- Module files: `{category}-{feature}.el` (e.g., `lang-python.el`, `tool-shell.el`)
- Categories: `core-`, `ui-`, `tool-`, `lang-`

### Variables and Functions
- Custom variables: `ian/{name}` (e.g., `ian/private-dir`)
- Custom functions: `ian/{verb}-{noun}` (e.g., `ian/cleanup-buffer`)

### Keybindings
All custom keybindings use `C-c` prefix with sub-prefixes:
- `C-c b` - Buffers
- `C-c d` - Dired
- `C-c e` - Editor
- `C-c g` - AI/GPT
- `C-c j` - Jump/navigation
- `C-c l` - LSP
- `C-c n` - Org-roam
- `C-c p` - Projectile
- `C-c t` - Terminal
- `C-c w` - Windows

### use-package Pattern
```elisp
(use-package package-name
  :straight t
  :defer t
  :hook (mode . function)
  :bind (("C-c x y" . command))
  :custom
  (variable value)
  :config
  (setup-code))
```

## Adding New Configuration

### New Package
Add to the appropriate module using `use-package`. Default is lazy-loading (`use-package-always-defer t`).

### New Language
1. Find the appropriate `lang-*.el` module or create a new one
2. Follow the module template pattern
3. Add `(require 'lang-newlang)` to `init.el` if creating new module

### Before Modifying
Check for conflicts:
```bash
# Check if keybinding exists
grep -r "C-c x" modules/

# Check if package already configured
grep -r "package-name" modules/
```

## Important Directories

- `var/` - Runtime data (backups, auto-saves, caches)
- `custom/` - User customizations (gitignored)
- `private/` - Credentials and secrets (gitignored)
- `straight/` - Package manager cache (gitignored)

## Notes

- Personal org-mode config in `org-pessoal.el` uses Portuguese
- See `docs/KEYBINDINGS.org` for full keybinding reference
- README.md contains additional architecture details and LLM modification guide
