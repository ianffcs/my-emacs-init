;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modular Emacs configuration.
;; Loads configuration from ~/.emacs.d/modules/

;;; Code:

;; ============================================================================
;; BOOTSTRAP
;; ============================================================================


;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load package management first
(require 'core-packages)

;; ============================================================================
;; CORE MODULES
;; ============================================================================

(require 'core-settings)    ; Base Emacs settings
(require 'core-os)          ; OS-specific configuration
(require 'core-utils)       ; Utility packages (which-key, helpful, etc.)
(require 'core-editor)      ; Editor behavior (parens, undo, whitespace)
(require 'core-ui)          ; Theme, fonts, modeline, icons
(require 'core-completion)  ; Vertico, Consult, Corfu, Embark
(require 'core-auth)        ; Authentication & security
(require 'core-session)     ; Session management & persistence

;; ============================================================================
;; UI MODULES
;; ============================================================================

(require 'ui-navigation)    ; Avy, search, jumping
(require 'ui-windows)       ; Window/workspace management
(require 'ui-buffers)       ; Buffer management (ibuffer)
(require 'ui-dashboard)     ; Dashboard startup screen

;; ============================================================================
;; TOOL MODULES
;; ============================================================================

(require 'tool-dev)         ; Git, LSP, Projectile, TRAMP
(require 'tool-shell)       ; Eshell, vterm, terminals
(require 'tool-dired)       ; File manager
(require 'tool-ai)          ; AI assistants
(require 'tool-comm)        ; Communication (Telega, IRC, RSS)
(require 'tool-media)       ; Media player (EMMS, MPD)

;; ============================================================================
;; LANGUAGE MODULES
;; ============================================================================

;; Lisp family
(require 'lang-lisp)        ; Clojure, Common Lisp, Scheme, Racket, Elisp

;; Systems languages
(require 'lang-systems)     ; C, C++, Rust, Go, Zig

;; JVM languages
(require 'lang-jvm)         ; Java, Kotlin, Scala, Groovy

;; BEAM languages
(require 'lang-beam)        ; Elixir, Erlang, Gleam

;; Scripting
(require 'lang-python)      ; Python

;; Web development
(require 'lang-web)         ; HTML, CSS, JavaScript, TypeScript

;; Text and writing
(require 'lang-org)         ; Org-mode, Roam, Babel, Export
(require 'lang-markdown)    ; Markdown, writing tools, spell check
(require 'lang-latex)       ; LaTeX, PDF viewing, BibTeX

;; DevOps
(require 'lang-ops)         ; Terraform, Ansible, Docker, K8s

;; Miscellaneous
(require 'lang-misc)        ; Other languages (Ruby, Lua, SQL, etc.)
(require 'lang-extra)       ; Extra languages (Dart, Julia, R, etc.)

;; ============================================================================
;; FINALIZE
;; ============================================================================

(provide 'init)
;;; init.el ends here
