# Ian's Emacs Configuration

A modular, well-organized Emacs configuration focused on software development,
writing, and productivity.

## Quick Start

```bash
git clone https://github.com/ianffcs/.emacs.d ~/.emacs.d
emacs
```

First launch will install packages automatically via `straight.el`.

## Architecture Overview

```
~/.emacs.d/
├── early-init.el          # Early initialization (GC, native-comp)
├── init.el                 # Main entry point, loads modules
├── modules/                # Modular configuration
│   ├── core-*.el          # Core functionality (8 files)
│   ├── ui-*.el            # UI components (4 files)
│   ├── tool-*.el          # Tools and utilities (6 files)
│   └── lang-*.el          # Language support (12 files)
├── docs/
│   └── KEYBINDINGS.org    # Comprehensive keybinding reference
└── custom/
    └── custom.el          # Customize settings (gitignored)
```

## Module Categories

### Core Modules (`core-*.el`)

| Module | Purpose |
|--------|---------|
| `core-packages.el` | Package management (straight.el, use-package) |
| `core-settings.el` | Basic Emacs settings |
| `core-os.el` | OS-specific configuration (macOS/Linux/Windows) |
| `core-utils.el` | Utility packages (which-key, helpful, try, google-this) |
| `core-editor.el` | Editing behavior (parens, undo, multiple-cursors) |
| `core-ui.el` | Theme, fonts, modeline, icons |
| `core-completion.el` | Completion framework (vertico, consult, corfu, embark) |
| `core-auth.el` | Authentication and security |
| `core-session.el` | Session management and persistence |

### UI Modules (`ui-*.el`)

| Module | Purpose |
|--------|---------|
| `ui-navigation.el` | Navigation (avy, search, jumping) |
| `ui-windows.el` | Window and workspace management |
| `ui-buffers.el` | Buffer management (ibuffer) |
| `ui-dashboard.el` | Startup dashboard |

### Tool Modules (`tool-*.el`)

| Module | Purpose |
|--------|---------|
| `tool-dev.el` | Development tools (git, LSP/eglot, projectile) |
| `tool-shell.el` | Terminals (vterm, eshell) |
| `tool-dired.el` | File manager |
| `tool-ai.el` | AI assistants (gptel, whisper, aider) |
| `tool-comm.el` | Communication (telega, IRC, RSS) |
| `tool-media.el` | Media player (EMMS) |

### Language Modules (`lang-*.el`)

| Module | Languages |
|--------|-----------|
| `lang-lisp.el` | Clojure, Common Lisp, Scheme, Racket, Elisp |
| `lang-systems.el` | C, C++, Rust, Go, Zig |
| `lang-jvm.el` | Java, Kotlin, Scala, Groovy |
| `lang-beam.el` | Elixir, Erlang, Gleam |
| `lang-python.el` | Python |
| `lang-web.el` | HTML, CSS, JavaScript, TypeScript |
| `lang-org.el` | Org-mode, Roam, Babel, Export |
| `lang-markdown.el` | Markdown, writing tools |
| `lang-latex.el` | LaTeX, PDF viewing |
| `lang-ops.el` | Terraform, Ansible, Docker, K8s |
| `lang-misc.el` | Ruby, Lua, Haskell, SQL |
| `lang-extra.el` | Dart, Julia, R, etc. |

## Key Features

- **Modern Completion**: Vertico + Consult + Corfu + Embark + Marginalia
- **LSP**: Eglot (built-in) with auto-configuration for 20+ languages
- **Git**: Magit + diff-hl + git-timemachine
- **AI Integration**: GPTel, Whisper, Aider, ChatGPT-shell
- **Org-mode**: Full GTD setup, Roam, Babel, Reveal.js presentations
- **Tree-sitter**: Automatic grammar installation for syntax highlighting
- **Formatting**: Apheleia for automatic code formatting

## Keybinding Philosophy

All custom keybindings use the `C-c` prefix:

| Prefix | Purpose |
|--------|---------|
| `C-c b` | Buffer operations |
| `C-c d` | Dired |
| `C-c e` | Editor operations |
| `C-c g` | AI/GPT commands |
| `C-c j` | Jump/navigation |
| `C-c l` | LSP/Eglot |
| `C-c n` | Org-roam notes |
| `C-c p` | Projectile |
| `C-c q` | Quit/utility |
| `C-c t` | Terminal |
| `C-c w` | Window management |

See `docs/KEYBINDINGS.org` for the complete reference.

---

# LLM Modification Guide

This section explains how to modify this configuration using an LLM (like Claude).

## Understanding the Structure

### File Naming Convention

```
{category}-{name}.el
```

- `core-*`: Essential functionality loaded first
- `ui-*`: Visual/interface components
- `tool-*`: External tool integrations
- `lang-*`: Programming language support

### Module Template

Every module follows this structure:

```elisp
;;; module-name.el --- Short Description -*- lexical-binding: t; -*-

;;; Commentary:
;; Longer description of what this module does.
;; List related modules or dependencies.

;;; Code:

;; ============================================================================
;; 1. SECTION NAME
;; ============================================================================

(use-package package-name
  :hook (mode . function)
  :bind (("key" . command))
  :custom
  (variable value)
  :config
  (setup-code))

;; ============================================================================
;; 2. NEXT SECTION
;; ============================================================================

;; ... more configuration ...

(provide 'module-name)
;;; module-name.el ends here
```

## Common Modification Tasks

### Adding a New Package

```elisp
(use-package new-package
  :straight t                    ; Install via straight.el
  :defer t                       ; Lazy load (default)
  :hook (some-mode . new-package-mode)
  :bind (("C-c x" . new-package-command))
  :custom
  (new-package-option value)
  :config
  (new-package-setup))
```

### Adding a Keybinding

For global keybindings:
```elisp
(global-set-key (kbd "C-c x y") #'my-command)
```

For mode-specific keybindings:
```elisp
(with-eval-after-load 'some-mode
  (define-key some-mode-map (kbd "C-c x") #'command))
```

Or with use-package:
```elisp
(use-package some-mode
  :bind (:map some-mode-map
              ("C-c x" . command)))
```

### Adding a New Language

1. Create `modules/lang-{name}.el`
2. Add to `init.el`: `(require 'lang-{name})`
3. Follow the module template

Example:
```elisp
;;; lang-newlang.el --- NewLang Support -*- lexical-binding: t; -*-

;;; Code:

(use-package newlang-mode
  :mode "\\.nl\\'"
  :hook (newlang-mode . eglot-ensure))

;; Add LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(newlang-mode . ("newlang-lsp"))))

;; Add formatter
(with-eval-after-load 'apheleia
  (setf (alist-get 'newlang-mode apheleia-mode-alist) '(newlangfmt)))

(provide 'lang-newlang)
;;; lang-newlang.el ends here
```

### Modifying Existing Configuration

**DO:**
- Use `Edit` tool to modify specific sections
- Keep changes minimal and focused
- Preserve existing structure and comments
- Check for keybinding conflicts before adding new ones

**DON'T:**
- Rewrite entire files unnecessarily
- Remove comments or section headers
- Add packages without checking if they already exist
- Create duplicate keybindings

### Checking for Conflicts

Before adding keybindings, check for conflicts:
```bash
grep -r "C-c x" modules/*.el
```

Before adding packages, check if already installed:
```bash
grep -r "use-package package-name" modules/*.el
```

## Key Files to Know

| File | When to Modify |
|------|----------------|
| `init.el` | Adding new modules |
| `early-init.el` | Performance tuning, native-comp |
| `core-packages.el` | Package manager settings |
| `core-settings.el` | Basic Emacs behavior |
| `core-editor.el` | Editing behavior, text manipulation |
| `core-ui.el` | Theme, fonts, visual settings |
| `core-completion.el` | Completion framework |
| `tool-dev.el` | LSP, git, development tools |

## Conventions

### Variable Naming
- Custom variables: `ian/{name}`
- Custom functions: `ian/{verb}-{noun}`

### Keybinding Prefixes
- `C-c` for custom commands
- `s-` (Super/Cmd) for macOS shortcuts
- Mode-specific bindings in mode maps

### Section Numbers
Each module uses numbered sections (1, 2, 3...) for organization.
Keep these when modifying.

## Testing Changes

After modifications:

1. **Syntax check**:
   ```bash
   emacs --batch -Q --eval "(find-file \"modules/file.el\") (check-parens)"
   ```

2. **Load test**:
   ```bash
   emacs --debug-init
   ```

3. **Check for warnings**:
   Look at `*Warnings*` buffer after startup

## Common Patterns

### Adding a hook
```elisp
(add-hook 'some-mode-hook #'my-function)
```

### Adding to a list
```elisp
(add-to-list 'some-list 'new-item)
```

### Conditional by OS
```elisp
(when (eq system-type 'darwin)
  ;; macOS specific
  )
```

### After package loads
```elisp
(with-eval-after-load 'package-name
  ;; runs after package loads
  )
```

## Troubleshooting

### Package won't install
- Check `straight-use-package-by-default` is `t`
- Try `M-x straight-pull-package`

### Keybinding not working
- Check with `C-h k` then press the key
- Look for conflicts with grep

### Mode not activating
- Check `auto-mode-alist` entries
- Verify file extension pattern

---

## License

This configuration is provided as-is for personal use.
