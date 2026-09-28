# Architecture

How the configuration starts up and how its modules depend on each other.

## Startup order

`init.el` is the whole hierarchy. No module `require`s another module; a module may rely on anything that loads before it and on nothing that loads after it.

```mermaid
flowchart TD
  E["early-init.el<br/>GC tuning, frame defaults, package.el off"] --> I["init.el<br/>adds modules/ to load-path"]
  I --> P["core-packages<br/>straight.el + use-package (root of everything)"]
  P --> C["core-* (8)<br/>settings, os, utils, editor, ui, completion, auth, session"]
  C --> U["ui-* (4)<br/>navigation, windows, buffers, dashboard"]
  U --> T["tool-* (7)<br/>dev, shell, dired, ai, comm, media, games"]
  T --> L["lang-* (14)<br/>lisp, carp, systems, jvm, beam, python, web,<br/>org, markdown, latex, ops, misc, extra, proof"]
  L --> S["emacs-startup-hook / after-init-hook<br/>theme sync, MCP servers, timers"]
```

Later groups may use earlier groups. Within a group, order follows `init.el`.

## What modules share

There are only two direct function-level dependencies. Everything else meets through variables and hooks that Emacs or a package owns.

```mermaid
flowchart LR
  subgraph fn["Function calls between modules"]
    CU["core-ui<br/>ian/icons-displayable-p"] --> CC[core-completion]
    CU --> UD[ui-dashboard]
    CU --> TD[tool-dired]
    CUT["core-utils<br/>ian/toggle-maximize-buffer"] -.bound by.-> UW[ui-windows]
  end
  subgraph state["Shared state"]
    LO["lang-org<br/>sets org-directory"] -.read.-> TA[tool-ai]
    LO -.read.-> TC[tool-comm]
    LO -.read.-> LX[lang-latex]
    LO -.read.-> UD2[ui-dashboard]
    LM["lang-* modules<br/>each fills eglot-server-programs,<br/>mode hooks, apheleia-mode-alist"] --> ED["eglot / apheleia<br/>(owned by their packages)"]
  end
```

- `org-directory` is read while `tool-ai` and `tool-comm` load, before `lang-org` sets it. It works because both use Emacs's default `~/org`. Changing the directory in `lang-org` alone would leave those readers on the old path.
- Timing hooks live in `core-ui` (theme sync on `after-init-hook` plus a timer) and `tool-ai` (MCP servers on `emacs-startup-hook`).

## Language toolchains

A language is registered entirely inside the `lang-*` module that owns it, using plain Emacs forms. There is no registration function.

```mermaid
flowchart LR
  subgraph mod["lang-systems.el (example)"]
    A["with-eval-after-load 'eglot<br/>add-to-list eglot-server-programs"]
    B["add-hook MODE-hook #'eglot-ensure<br/>(gated by executable-find for newer servers)"]
    F["with-eval-after-load 'apheleia<br/>setf apheleia-mode-alist / apheleia-formatters"]
  end
  A --> EG[eglot-server-programs]
  B --> H[MODE-hook]
  F --> AP[apheleia-mode-alist]
```

Each toolchain lists its `-mode` and `-ts-mode` twins explicitly. See `test/lang-lsp-test.el` for the invariants that are checked.

| Module | Registers |
|---|---|
| `lang-lisp` | Clojure (project-root guard), Racket |
| `lang-systems` | C, C++, Rust, Go, Zig |
| `lang-jvm` | Kotlin, Scala (Java goes through `eglot-java`) |
| `lang-beam` | Elixir, Erlang, Gleam |
| `lang-python`, `lang-web` | Python; JavaScript, TypeScript (Eglot's built-in servers) |
| `lang-ops` | Terraform, Dockerfile, Nix |
| `lang-misc` | Haskell, Bash, Ruby, Lua |
| `lang-extra` | Dart, R, Julia |
| `lang-proof` | Idris 2 (Lean uses `lsp-mode` on purpose) |

## Autostart rule

- Languages that autostarted before this layout (Python, JS/TS, Rust, Go, C/C++, Java, Elixir, Haskell, Terraform, shell, Dockerfile, Nix, Clojure, Dart, Idris) hook `eglot-ensure` unconditionally.
- Newer ones (Kotlin, Scala, Ruby, Lua, Zig, Racket, Gleam, Erlang, R, Julia) hook only if the server binary is on `PATH` when the module loads. Installing one mid-session needs a restart, and a server that only exists inside a project's `envrc` environment is missed.
