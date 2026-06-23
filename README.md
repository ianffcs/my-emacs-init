# Ian's Emacs Configuration

A modular Emacs configuration using `straight.el` + `use-package`. Focused on software development, writing, AI tooling, and formal verification.

## Quick Start

```bash
git clone https://github.com/ianffcs/.emacs.d ~/.emacs.d
emacs
```

First launch installs all packages automatically via `straight.el`. Be patient — it takes a few minutes.

## File Structure

```
~/.emacs.d/
├── early-init.el          # GC tuning, native-comp, GUI suppression
├── init.el                # Entry point — loads modules in order
├── modules/               # All configuration (33 .el files)
│   ├── core-*.el          # 8 files: packages, settings, editor, UI, completion…
│   ├── ui-*.el            # 4 files: navigation, windows, buffers, dashboard
│   ├── tool-*.el          # 7 files: git/LSP, shells, dired, AI, comms, media, games
│   └── lang-*.el          # 13 files: language support
├── docs/
│   └── KEYBINDINGS.org    # Full keybinding reference
├── snippets/              # YASnippet snippets
└── custom/
    └── custom.el          # Customize output (gitignored)
```

## Modules

### Core (`core-*.el`)

| Module | Purpose |
|--------|---------|
| `core-packages.el` | straight.el bootstrap, use-package, gcmh, treesit-auto |
| `core-settings.el` | Fundamental Emacs settings, auto-mode associations |
| `core-os.el` | macOS / Linux / Windows / WSL specifics, PATH setup |
| `core-utils.el` | which-key, helpful, yasnippet, crux, gcmh, google-this |
| `core-editor.el` | Parens, undo-fu, multiple-cursors, expand-region, hl-todo |
| `core-ui.el` | Modus themes, fonts, ligatures, nerd-icons, doom-modeline |
| `core-completion.el` | Vertico, Orderless, Marginalia, Consult, Embark, Corfu, Cape |
| `core-auth.el` | GPG, pinentry, auth-source, password-store |
| `core-session.el` | savehist, recentf, saveplace, desktop, alerts |

### UI (`ui-*.el`)

| Module | Purpose |
|--------|---------|
| `ui-navigation.el` | Avy, goto-chg, imenu-list, bm bookmarks, wgrep, deadgrep, rg |
| `ui-windows.el` | winner, windmove, ace-window, tab-bar, popper, workspace-hud |
| `ui-buffers.el` | ibuffer, uniquify, midnight cleanup, vlf |
| `ui-dashboard.el` | Startup dashboard |

### Tools (`tool-*.el`)

| Module | Purpose |
|--------|---------|
| `tool-dev.el` | Magit, diff-hl, Eglot (LSP), Projectile, apheleia, envrc, dape |
| `tool-shell.el` | vterm, multi-vterm, eshell, eat, shell-pop, comint |
| `tool-dired.el` | dired + subtree sidebar, dired-narrow, diredfl, dired-ranger |
| `tool-ai.el` | gptel, minuet, whisper, aider, org-ai, MCP, ellama, chatgpt-shell |
| `tool-comm.el` | Telega (Telegram), Circe (IRC), Elfeed (RSS) |
| `tool-media.el` | EMMS media player |
| `tool-games.el` | NetHack |

### Languages (`lang-*.el`)

| Module | Languages |
|--------|-----------|
| `lang-lisp.el` | Clojure, Common Lisp, Scheme, Racket, Fennel, Elisp |
| `lang-systems.el` | C, C++, Rust, Go, Zig |
| `lang-jvm.el` | Java, Kotlin, Scala, Groovy |
| `lang-beam.el` | Elixir, Erlang, Gleam |
| `lang-python.el` | Python (pyright, ruff, black) |
| `lang-web.el` | HTML, CSS, JavaScript, TypeScript, JSON, YAML |
| `lang-org.el` | Org-mode, Org-roam, Babel, Reveal.js, Denote |
| `lang-markdown.el` | Markdown, writing tools, spell check |
| `lang-latex.el` | LaTeX, PDF viewing, BibTeX |
| `lang-ops.el` | Terraform, Ansible, Docker, Kubernetes, Nix |
| `lang-misc.el` | Ruby, Lua, Haskell, SQL, Bash, R |
| `lang-extra.el` | Dart/Flutter, Hy, Forth, Julia |
| `lang-proof.el` | Agda, Idris 2, Lean 4, TLA+ |

## Key Features

- **Completion**: Vertico + Corfu + Orderless + Consult + Embark + Cape + Marginalia
- **LSP**: Eglot (built-in) for 25+ languages with workspace configuration
- **Formatting**: Apheleia automatic on-save formatting for all major languages
- **Git**: Magit + diff-hl + git-timemachine + browse-at-remote
- **AI**: GPTel, Minuet (inline completion), Aider, Whisper, MCP servers, org-ai
- **Org**: GTD workflow, Org-roam knowledge base, Babel literate programming, Reveal.js slides, Denote notes
- **Tree-sitter**: Auto grammar installation via treesit-auto
- **Proof Assistants**: Agda, Idris 2, Lean 4, TLA+
- **Terminals**: vterm + eshell + eat + shell-pop
- **Themes**: Modus themes with auto light/dark switching by time of day

## Keybinding Philosophy

All custom bindings live under `C-c`:

| Prefix | Domain |
|--------|--------|
| `C-c b` | Buffers |
| `C-c d` | Dired |
| `C-c e` | Editor operations |
| `C-c f` | File utilities |
| `C-c g` | AI / Git |
| `C-c j` | Jump / navigation |
| `C-c l` | LSP (Eglot) |
| `C-c n` | Org-roam notes |
| `C-c p` | Projectile |
| `C-c q` | Quit / utility |
| `C-c s` | Search menu |
| `C-c t` | Terminals |
| `C-c w` | Windows |
| `C-c y` | Snippets |

See `docs/KEYBINDINGS.org` for the full reference.

---

## Modification Guide

### Adding a Package

Add to the appropriate module file:

```elisp
(use-package new-package
  :straight t
  :defer t                              ; omit if you need :demand t
  :hook (some-mode . new-package-mode)
  :bind ("C-c x" . new-package-command)
  :custom
  (new-package-option value)
  :config
  (setup-code))
```

### Adding a Language

1. Find the right `lang-*.el` or create `modules/lang-newlang.el`
2. Add `(require 'lang-newlang)` to `init.el`
3. Follow this pattern:

```elisp
;;; lang-newlang.el --- NewLang support -*- lexical-binding: t; -*-
;;; Code:

(use-package newlang-mode
  :straight t
  :mode "\\.nl\\'"
  :hook (newlang-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(newlang-mode . ("newlang-lsp"))))

(with-eval-after-load 'apheleia
  (setf (alist-get 'newlang-fmt apheleia-formatters) '("newlangfmt"))
  (setf (alist-get 'newlang-mode apheleia-mode-alist) '(newlang-fmt)))

(provide 'lang-newlang)
;;; lang-newlang.el ends here
```

### Checking for Conflicts

```bash
# Before adding a keybinding
grep -r "C-c x" modules/

# Before adding a package
grep -r "package-name" modules/
```

### Testing Changes

```bash
# Syntax check
emacs --batch -Q --eval "(find-file \"modules/file.el\") (check-parens)"

# Full startup test
emacs --debug-init
```

Check `*Warnings*` buffer after startup.

### Conventions

- Functions: `ian/{verb}-{noun}` (e.g. `ian/cleanup-buffer`)
- Variables: `ian/{name}` (e.g. `ian/private-dir`)
- Custom options: `defcustom` under `:group 'ian`
- Each module ends with `(provide 'module-name)`

### Key Files

| File | Edit when… |
|------|-----------|
| `early-init.el` | Changing startup performance settings |
| `init.el` | Adding or reordering modules |
| `core-packages.el` | Changing straight.el / use-package behaviour |
| `core-ui.el` | Theme, fonts, modeline |
| `core-completion.el` | Completion framework |
| `core-editor.el` | Editing behaviour, text manipulation |
| `core-os.el` | OS-specific paths, keybindings |
| `tool-dev.el` | LSP servers, git, formatters |
| `tool-ai.el` | AI backends, models, MCP |
