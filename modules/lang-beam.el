;;; lang-beam.el --- BEAM Languages (Elixir, Erlang) -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for BEAM VM languages: Elixir, Erlang, Gleam.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. ELIXIR
;; ============================================================================

(use-package elixir-mode
  :disabled t)

(use-package elixir-ts-mode
  :straight (:type built-in)
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode)
         ("mix\\.lock\\'" . elixir-ts-mode))
  :hook ((elixir-ts-mode . subword-mode)))

;; Mix integration
(use-package mix
  :after elixir-ts-mode
  :hook (elixir-ts-mode . mix-minor-mode)
  :bind (:map elixir-ts-mode-map
              ("C-c m c" . mix-compile)
              ("C-c m t" . mix-test)
              ("C-c m T" . mix-test-current-test)
              ("C-c m f" . mix-test-current-buffer)
              ("C-c m r" . mix-run)
              ("C-c m d" . mix-deps-get)
              ("C-c m x" . mix-execute-task)))

;; Alchemist (alternative to mix.el)
(use-package alchemist
  :disabled  ; Using mix.el instead
  :after elixir-ts-mode
  :hook (elixir-ts-mode . alchemist-mode))

;; Exunit tests
(use-package exunit
  :after elixir-ts-mode
  :hook (elixir-ts-mode . exunit-mode)
  :bind (:map elixir-ts-mode-map
              ("C-c e t" . exunit-verify)
              ("C-c e T" . exunit-verify-single)
              ("C-c e a" . exunit-verify-all)
              ("C-c e r" . exunit-rerun)))

;; Flycheck for Elixir
(use-package flycheck-credo
  :after (flycheck elixir-ts-mode)
  :hook (elixir-ts-mode . flycheck-credo-setup))

;; ============================================================================
;; 2. ERLANG
;; ============================================================================

(use-package erlang
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.app\\.src\\'" . erlang-mode)
         ("rebar\\.config\\'" . erlang-mode)
         ("sys\\.config\\'" . erlang-mode)
         ("relup\\'" . erlang-mode)
         ("app\\.config\\'" . erlang-mode))
  :custom
  (erlang-indent-level 4)
  :config
  ;; Erlang root directory
  (when (file-directory-p "/usr/local/lib/erlang")
    (setq erlang-root-dir "/usr/local/lib/erlang")))

;; ============================================================================
;; 3. GLEAM
;; ============================================================================

(use-package gleam-mode
  :straight (:host github :repo "gleam-lang/gleam-mode")
  :mode "\\.gleam\\'"
  :hook (gleam-mode . subword-mode))

;; Gleam tree-sitter
(use-package gleam-ts-mode
  :straight (:host github :repo "gleam-lang/gleam-mode")
  :mode "\\.gleam\\'")

;; ============================================================================
;; 4. HEEX (Phoenix templates)
;; ============================================================================

(use-package heex-ts-mode
  :straight (:type built-in)
  :mode "\\.heex\\'")

;; ============================================================================
;; 5. PHOENIX FRAMEWORK SUPPORT
;; ============================================================================

(defun ian/phoenix-project-p ()
  "Check if the current project is a Phoenix project."
  (and (projectile-project-p)
       (file-exists-p (expand-file-name "mix.exs" (projectile-project-root)))
       (with-temp-buffer
         (insert-file-contents (expand-file-name "mix.exs" (projectile-project-root)))
         (search-forward "phoenix" nil t))))

(defun ian/phoenix-server ()
  "Start Phoenix server in a compilation buffer."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix phx.server")))

(defun ian/phoenix-routes ()
  "Show Phoenix routes."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix phx.routes")))

(defun ian/phoenix-iex ()
  "Start IEx with Phoenix loaded."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (run-elixir "iex -S mix phx.server")))

;; Phoenix keybindings
(with-eval-after-load 'elixir-ts-mode
  (define-key elixir-ts-mode-map (kbd "C-c P s") #'ian/phoenix-server)
  (define-key elixir-ts-mode-map (kbd "C-c P r") #'ian/phoenix-routes)
  (define-key elixir-ts-mode-map (kbd "C-c P i") #'ian/phoenix-iex))

;; ============================================================================
;; 6. INF-ELIXIR (IEx REPL)
;; ============================================================================

(use-package inf-elixir
  :after elixir-ts-mode
  :bind (:map elixir-ts-mode-map
              ("C-c C-z" . inf-elixir)
              ("C-c C-l" . inf-elixir-send-line)
              ("C-c C-r" . inf-elixir-send-region)
              ("C-c C-b" . inf-elixir-send-buffer)))

;; ============================================================================
;; 7. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Elixir
  (setf (alist-get 'mix-format apheleia-formatters)
        '("mix" "format" "-"))
  (setf (alist-get 'elixir-ts-mode apheleia-mode-alist) '(mix-format))

  ;; Erlang
  (setf (alist-get 'erlfmt apheleia-formatters)
        '("erlfmt" "-"))
  (setf (alist-get 'erlang-mode apheleia-mode-alist) '(erlfmt))

  ;; Gleam
  (setf (alist-get 'gleam-format apheleia-formatters)
        '("gleam" "format" "--stdin"))
  (setf (alist-get 'gleam-mode apheleia-mode-alist) '(gleam-format)))

;; ============================================================================
;; 9. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/elixir-menu ()
    "Elixir development commands"
    ["Mix"
     ("c" "Compile" mix-compile)
     ("d" "Deps get" mix-deps-get)
     ("x" "Execute task" mix-execute-task)]
    ["Test"
     ("t" "Test all" mix-test)
     ("T" "Test current" mix-test-current-test)
     ("f" "Test file" mix-test-current-buffer)
     ("r" "Rerun" exunit-rerun)]
    ["Phoenix"
     ("s" "Server" ian/phoenix-server)
     ("R" "Routes" ian/phoenix-routes)
     ("i" "IEx" ian/phoenix-iex)]
    ["REPL"
     ("z" "IEx" inf-elixir)
     ("l" "Send line" inf-elixir-send-line)
     ("e" "Send region" inf-elixir-send-region)
     ("b" "Send buffer" inf-elixir-send-buffer)])

  (with-eval-after-load 'elixir-ts-mode
    (define-key elixir-ts-mode-map (kbd "C-c C-m") #'ian/elixir-menu)))

(provide 'lang-beam)
;;; lang-beam.el ends here
