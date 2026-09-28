# Architecture

How the configuration starts up and how its modules depend on each other.

## Startup order

`init.el` is the whole hierarchy. No module `require`s another module; a module may rely on anything that loads before it and on nothing that loads after it.

```mermaid
flowchart TD
  E["early-init.el<br/>GC tuning, frame defaults, package.el off"] --> I["init.el<br/>adds modules/ to load-path"]
  I --> P["core-packages<br/>straight.el + use-package (root of everything)"]
  P --> C["core-* (9)<br/>settings, os, utils, editor, ui, completion, auth, session"]
  C --> U["ui-* (4)<br/>navigation, windows, buffers, dashboard"]
  U --> T["tool-* (9)<br/>dev, shell, dired, chat, speech, MCP, comm, media, games"]
  T --> L["lang-* (14)<br/>lisp, carp, systems, jvm, beam, python, web,<br/>org, markdown, latex, ops, misc, extra, proof"]
  L --> S["emacs-startup-hook / after-init-hook<br/>theme sync, MCP servers, timers"]
```

Later groups may use earlier groups. Within a group, order follows `init.el`.

## What modules share

Direct function calls connect a few modules; other coordination uses variables and hooks that Emacs or a package owns.

```mermaid
flowchart LR
  subgraph fn["Function calls between modules"]
    CU["core-ui<br/>ian/icons-displayable-p"] --> CC[core-completion]
    CU --> UD[ui-dashboard]
    CU --> TD[tool-dired]
    CUT["core-utils<br/>ian/toggle-maximize-buffer"] -.bound by.-> UW[ui-windows]
    CA["core-auth<br/>ian/get-key"] --> TCH[tool-chat]
    CA --> TC[tool-comm]
  end
  subgraph state["Shared state"]
    CS["core-settings<br/>sets org-directory"] -.read.-> TCH[tool-chat]
    CS -.read.-> TM[tool-mcp]
    CS -.read.-> TC[tool-comm]
    CS -.read.-> LX[lang-latex]
    CS -.read.-> UD2[ui-dashboard]
    LM["lang-* modules<br/>each fills eglot-server-programs,<br/>mode hooks, apheleia-mode-alist"] --> ED["eglot / apheleia<br/>(owned by their packages)"]
  end
```

- `core-settings` establishes `org-directory` before UI, tool, and language modules load. Consumers derive their paths from this shared root; `lang-org` configures Org behavior and derived Org paths without resetting the root.
- Timing hooks live in `core-ui` (theme sync on `after-init-hook` plus a timer) and `tool-mcp` (MCP servers on `emacs-startup-hook`).

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
