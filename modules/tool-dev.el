;;; tool-dev.el --- Development Tools (LSP, Git, Projectile, TRAMP) -*- lexical-binding: t; -*-

;;; Commentary:
;; Development environment setup: version control, LSP, debugging, remote.
;; NOTE: Terminals (vterm, eat, eshell) moved to tool-shell.el
;; NOTE: rainbow-delimiters, aggressive-indent in core-editor.el
;; NOTE: editorconfig in core-editor.el
;; Migrated from README.org literate config.

;;; Code:

(require 'seq)

;; ============================================================================
;; 1. PROJECT MANAGEMENT
;; ============================================================================

(use-package project
  :straight (:type built-in)
  :defer t
  :custom
  (project-vc-extra-root-markers '(".project" "package.json" ".git" "deps.edn" "project.clj")))

(use-package projectile
  :diminish
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recentf)
  (projectile-auto-discover nil)
  :config
  (setq projectile-project-search-path '("~/src/" "~/work/" ("~/github" . 1)))

  ;; Integrate with built-in project.el
  (add-hook 'project-find-functions #'project-projectile)

  ;; Ignored directories
  (dolist (dir '("node_modules" ".git" "target" "dist" ".clj-kondo" ".cpcache" ".lsp" "__pycache__" ".lake" "build"))
    (add-to-list 'projectile-globally-ignored-directories dir))

  ;; Ignored files
  (dolist (file '(".DS_Store" "*.pyc" "*.elc" "*.class"))
    (add-to-list 'projectile-globally-ignored-files file)))


(defun project-projectile (dir)
  "Return projectile project for DIR if it exists."
  (when (and (fboundp 'projectile-project-root)
             (projectile-project-p dir))
    (cons 'projectile (projectile-project-root dir))))

(cl-defmethod project-root ((project (head projectile)))
  "Return root for PROJECT of type projectile."
  (cdr project))

(use-package projectile-ripgrep
  :after projectile)

;; ============================================================================
;; 2. VERSION CONTROL (Git)
;; ============================================================================

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-options '("--word-diff"))
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 15))
  (magit-save-repository-buffers 'dontask)
  (magit-revert-buffers 'silent)
  (magit-commit-arguments '("--verbose"))
  (magit-process-popup-time 10)
  (magit-auto-revert-mode nil)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; Git gutter
(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode))

;; Step through file history
(use-package git-timemachine
  :commands git-timemachine)

;; Open file/region on GitHub/GitLab
(use-package browse-at-remote
  :commands browse-at-remote)

;; Generate .gitignore files
(use-package gitignore-templates
  :commands gitignore-templates-insert)

;; Edit with $EDITOR
(use-package with-editor
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command)))

;; EDIFF
(use-package ediff
  :straight (:type built-in)
  :defer t
  :config
  (setq ediff-highlight-all-diffs nil
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally))

;; ============================================================================
;; 3. LANGUAGE SERVER PROTOCOL (Eglot)
;; ============================================================================

;; Helper function for workspace configuration
(defun ian/eglot-add-workspace-config (key value)
  "Add KEY with VALUE to eglot-workspace-configuration."
  (setq-default eglot-workspace-configuration
                (plist-put (default-value 'eglot-workspace-configuration)
                           key value)))

(defvar ian/clojure-project-root-files
  '("deps.edn" "project.clj" "build.boot" "bb.edn" "shadow-cljs.edn")
  "Files that identify a directory as a Clojure project root.")

(defun ian/clojure-project-root ()
  "Return a Clojure project root for `default-directory', or nil."
  (seq-some (lambda (file)
              (locate-dominating-file default-directory file))
            ian/clojure-project-root-files))

(defun ian/eglot-ensure-clojure ()
  "Start Clojure Eglot only inside a concrete Clojure project."
  (when-let* ((_ (executable-find "clojure-lsp"))
              (root (ian/clojure-project-root)))
    (unless (file-equal-p root (expand-file-name "~"))
      (eglot-ensure))))

(use-package eglot
  :defer t
  :hook ((clojure-mode . ian/eglot-ensure-clojure)
         (clojure-ts-mode . ian/eglot-ensure-clojure)
         (clojurec-mode . ian/eglot-ensure-clojure)
         (clojurescript-mode . ian/eglot-ensure-clojure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (elixir-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (terraform-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure)
         (dockerfile-mode . eglot-ensure)
         (dockerfile-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l F" . eglot-format-buffer)
              ("C-c l d" . eldoc-box-help-at-point)
              ("C-c l ." . xref-find-definitions)
              ("C-c l ," . xref-find-references)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l q" . eglot-shutdown)
              ("C-c l Q" . eglot-shutdown-all)
              ("C-c l R" . eglot-reconnect))
  :config
  (setq eglot-autoshutdown t
        eglot-connect-timeout 60
        eglot-extend-to-xref t
        eglot-report-progress nil
        eglot-events-buffer-size 0
        eglot-sync-connect nil
        eldoc-echo-area-use-multiline-p nil)

  ;; Clojure LSP
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojure-ts-mode clojurec-mode
                               clojure-ts-clojurec-mode clojurescript-mode
                               clojure-ts-clojurescript-mode)
                 . ("clojure-lsp")))

  ;; Elixir
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls")))

  ;; Erlang
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("erlang_ls")))

  ;; Gleam
  (add-to-list 'eglot-server-programs
               '((gleam-mode gleam-ts-mode) . ("gleam" "lsp")))

  ;; Haskell
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))

  ;; Dart
  (add-to-list 'eglot-server-programs
               '(dart-mode . ("dart" "language-server" "--protocol=lsp")))

  ;; R
  (add-to-list 'eglot-server-programs
               '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()")))

  ;; Julia
  (add-to-list 'eglot-server-programs
               '(julia-mode . ("julia" "--startup-file=no" "--history-file=no"
                               "-e" "using LanguageServer; runserver()")))

  ;; Racket
  (add-to-list 'eglot-server-programs
               '(racket-mode . ("racket" "-l" "racket-langserver")))

  ;; Ruby
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("solargraph" "stdio")))

  ;; Lua
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server")))

  ;; Bash
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))

  ;; Kotlin
  (add-to-list 'eglot-server-programs
               '((kotlin-mode kotlin-ts-mode) . ("kotlin-language-server")))

  ;; Scala
  (add-to-list 'eglot-server-programs
               '((scala-mode scala-ts-mode) . ("metals")))

  ;; C/C++
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd" "--background-index" "--clang-tidy")))

  ;; Rust
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))

  ;; Go
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))

  ;; Zig
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls")))

  ;; Python
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("pyright-langserver" "--stdio")))

  ;; Terraform
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))

  ;; Dockerfile
  (add-to-list 'eglot-server-programs
               '((dockerfile-mode dockerfile-ts-mode)
                 . ("docker-langserver" "--stdio")))

  ;; Nix
  (add-to-list 'eglot-server-programs
               '((nix-mode nix-ts-mode) . ("nil")))

  (when (fboundp 'ian/eglot-add-workspace-config)
    (ian/eglot-add-workspace-config
     :clojure-lsp '(:lens (:enable t)
                          :semantic-tokens (:enable t)
                          :source-paths ["src" "test" "dev"]))
    (ian/eglot-add-workspace-config
     :rust-analyzer '(:check (:command "clippy")
                      :cargo (:buildScripts (:enable t))
                      :procMacro (:enable t)))
    (ian/eglot-add-workspace-config
     :gopls '(:staticcheck t
            :usePlaceholders t
            :completeUnimported t))
    (ian/eglot-add-workspace-config
     :python.analysis '(:autoSearchPaths t
                        :useLibraryCodeForTypes t
                        :diagnosticMode "workspace"
                        :typeCheckingMode "basic"))))

;; Cape + Eglot: merge eglot's capf with cape sources so file/dabbrev
;; completion still works in LSP buffers (eglot otherwise replaces all capfs).
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'eglot-completion-at-point
                               #'cape-file
                               #'cape-dabbrev)))))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-s s" . consult-eglot-symbols)))

(use-package eldoc-box
  :commands eldoc-box-help-at-point
  :custom
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 400))

;; ============================================================================
;; 4. SYNTAX CHECKING (Flymake)
;; ============================================================================

(use-package flycheck
  :defer t)

(use-package flymake
  :straight (:type built-in)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t))

;; ============================================================================
;; 5. DEBUGGING (DAP Mode)
;; ============================================================================

(use-package dape
  :commands dape
  :config
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))
  (add-hook 'dape-compile-compile-hooks 'kill-buffer))

;; ============================================================================
;; 6. FALLBACK NAVIGATION (Dumb-jump)
;; ============================================================================

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg))

;; ============================================================================
;; 7. AUTO-FORMATTING (Apheleia)
;; ============================================================================

(use-package apheleia
  :hook (after-init . apheleia-global-mode))

;; ============================================================================
;; 8. TREE-SITTER (configured in core-packages.el)
;; ============================================================================

;; treesit-auto is already demand-loaded and configured in core-packages.el.
;; Only set the install preference here.
(with-eval-after-load 'treesit-auto
  (setq treesit-auto-install 'prompt))

;; ============================================================================
;; 9. DIRENV/ENVRC
;; ============================================================================

;; envrc is the correct Emacs-native direnv integration; direnv.el is redundant.
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; ============================================================================
;; 10. REST CLIENT
;; ============================================================================

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package ob-restclient
  :after org)

;; ============================================================================
;; 11. PLANTUML & MERMAID
;; ============================================================================

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :config
  (let* ((plantuml-directory (concat user-emacs-directory "private/"))
         (plantuml-target (concat plantuml-directory "plantuml.jar")))
    (unless (file-exists-p plantuml-directory)
      (make-directory plantuml-directory t))
    (setq org-plantuml-jar-path plantuml-target
          plantuml-jar-path plantuml-target
          plantuml-default-exec-mode 'jar
          plantuml-output-type "svg")
    (unless (file-exists-p plantuml-target)
      (message "plantuml.jar missing — run M-x ian/plantuml-download to install"))))

(defun ian/plantuml-download ()
  "Async download of plantuml.jar."
  (interactive)
  (let* ((dir (concat user-emacs-directory "private/"))
         (target (concat dir "plantuml.jar"))
         (url "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))
    (message "Downloading plantuml.jar...")
    (url-retrieve url
                  (lambda (status)
                    (unless (plist-get status :error)
                      (write-region (point-min) (point-max) target)
                      (message "plantuml.jar downloaded to %s" target))))))

(use-package mermaid-mode
  :mode "\\.mmd\\'")

(use-package ob-mermaid
  :after org)

;; ============================================================================
;; 13. TRAMP (Remote Development)
;; ============================================================================

(use-package tramp
  :straight (:type built-in)
  :defer t
  :custom
  (tramp-verbose 1)
  (tramp-default-method "ssh")
  (tramp-default-remote-shell "/bin/bash")
  (tramp-encoding-shell "/bin/bash")
  (tramp-connection-timeout 10)
  (remote-file-name-inhibit-cache nil)
  (tramp-completion-reread-directory-timeout nil)
  (vc-ignore-dir-regexp
   (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-auto-save-directory (expand-file-name "tramp-autosave" user-emacs-directory))
  :config
  (customize-set-variable 'tramp-use-ssh-controlmaster-options t)
  (customize-set-variable 'tramp-ssh-controlmaster-options
                          (concat
                           "-o ControlMaster=auto "
                           "-o ControlPath='~/.ssh/sockets/%%r@%%h-%%p' "
                           "-o ControlPersist=600 "))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/usr/local/bin")
  (add-to-list 'tramp-remote-path "~/bin")
  (add-to-list 'tramp-remote-path "~/.local/bin"))

;; TRAMP helper functions
(defun ian/sudo-edit (&optional arg)
  "Edit current file as root."
  (interactive "P")
  (let ((file (if arg
                  (read-file-name "Edit as root: ")
                (or buffer-file-name
                    (error "Buffer is not visiting a file")))))
    (find-file
     (if (file-remote-p file)
         (let* ((vec (tramp-dissect-file-name file))
                (sudo-vec (make-tramp-file-name
                           :method "sudo"
                           :user (tramp-file-name-user vec)
                           :domain (tramp-file-name-domain vec)
                           :host (tramp-file-name-host vec)
                           :port (tramp-file-name-port vec)
                           :localname (tramp-file-name-localname vec)
                           :hop (tramp-file-name-hop vec))))
           (tramp-make-tramp-file-name sudo-vec))
       (concat "/sudo:root@localhost:" file)))))

(defun ian/ssh-connect (host)
  "Connect to HOST via SSH."
  (interactive "sHost: ")
  (find-file (format "/ssh:%s:" host)))

(global-set-key (kbd "C-c T e") #'ian/sudo-edit)
(global-set-key (kbd "C-c T s") #'ian/ssh-connect)

;; ============================================================================
;; 14. CODE FOLDING
;; ============================================================================

(use-package hideshow
  :straight (:type built-in)
  :hook (prog-mode . hs-minor-mode))

;; ============================================================================
;; 15. MISC DEV TOOLS
;; ============================================================================

;; smart-shift is in core-editor.el (canonical location for text manipulation)

(use-package eldoc
  :straight (:type built-in)
  :diminish
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))

(use-package compile
  :straight (:type built-in)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  :config
  (require 'ansi-color)
  (defun ian/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'ian/colorize-compilation-buffer))

(provide 'tool-dev)
;;; tool-dev.el ends here
