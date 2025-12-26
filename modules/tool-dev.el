;;; tool-dev.el --- Development Tools (LSP, Git, Projectile, TRAMP) -*- lexical-binding: t; -*-

;;; Commentary:
;; Development environment setup: version control, LSP, debugging, remote.
;; NOTE: Terminals (vterm, eat, eshell) moved to tool-shell.el
;; NOTE: rainbow-delimiters, aggressive-indent in core-editor.el
;; NOTE: editorconfig in core-editor.el
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. PROJECT MANAGEMENT
;; ============================================================================

(use-package project
  :straight (:type built-in)
  :defer t
  :custom
  (project-vc-extra-root-markers '(".project" "package.json" ".git" "deps.edn" "project.clj")))

(use-package projectile
  :demand t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-sort-order 'recentf)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/src/" "~/work/" ("~/github" . 1)))

  ;; Integrate with built-in project.el
  (add-hook 'project-find-functions #'project-projectile)

  ;; Ignored directories
  (dolist (dir '("node_modules" ".git" "target" "dist" ".clj-kondo" ".cpcache" ".lsp" "__pycache__"))
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
  :bind ("C-c g t" . git-timemachine))

;; Open file/region on GitHub/GitLab
(use-package browse-at-remote
  :bind ("C-c g b" . browse-at-remote))

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

(use-package eglot
  :defer t
  :hook ((clojure-mode . eglot-ensure)
         (clojure-ts-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
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
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout 600)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect 0)
  (eldoc-echo-area-use-multiline-p nil)
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
  (fset #'jsonrpc--log-event #'ignore)

  ;; Clojure LSP
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurec-mode clojurescript-mode clojure-ts-mode)
                 . ("clojure-lsp")))

  ;; Elixir
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode) . ("elixir-ls")))

  ;; Haskell
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))

  ;; NOTE: Server programs for C/C++, Rust, Go, Python, Terraform, etc.
  ;; are defined in their respective lang-*.el modules

  ;; Centralized workspace configuration for all languages
  (setq-default eglot-workspace-configuration
                '(:clojure-lsp (:lens (:enable t)))))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-s s" . consult-eglot-symbols)))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 400))

;; ============================================================================
;; 4. SYNTAX CHECKING (Flymake)
;; ============================================================================

(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics))
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
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        dumb-jump-force-searcher 'rg))

;; ============================================================================
;; 7. AUTO-FORMATTING (Apheleia)
;; ============================================================================

(use-package apheleia
  :hook (after-init . apheleia-global-mode))

;; ============================================================================
;; 8. TREE-SITTER
;; ============================================================================

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; ============================================================================
;; 9. DIRENV/ENVRC
;; ============================================================================

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package direnv
  :config
  (direnv-mode))

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
  (let ((plantuml-directory (concat user-emacs-directory "private/"))
        (plantuml-link "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))
    (unless (file-exists-p plantuml-directory)
      (make-directory plantuml-directory t))
    (let ((plantuml-target (concat plantuml-directory "plantuml.jar")))
      (unless (file-exists-p plantuml-target)
        (message "Downloading plantuml.jar...")
        (url-copy-file plantuml-link plantuml-target t))
      (setq org-plantuml-jar-path plantuml-target
            plantuml-jar-path plantuml-target
            plantuml-default-exec-mode 'jar
            plantuml-output-type "svg"))))

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
  (setq tramp-use-ssh-controlmaster-options nil)
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
         (let ((vec (tramp-dissect-file-name file)))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-domain vec)
            (tramp-file-name-host vec)
            (tramp-file-name-port vec)
            (tramp-file-name-localname vec)))
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
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c h h" . hs-toggle-hiding)
              ("C-c h a" . hs-hide-all)
              ("C-c h s" . hs-show-all)
              ("C-c h l" . hs-hide-level)))

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
  :bind (("C-c c c" . compile)
         ("C-c c r" . recompile))
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

