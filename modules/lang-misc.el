;;; lang-misc.el --- Miscellaneous Languages -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for various programming languages not covered by other lang-* modules.
;; NOTE: Rust, Go are in lang-systems.el
;; NOTE: Elixir, Erlang are in lang-beam.el
;; NOTE: Terraform, Docker, Nix are in lang-ops.el
;; NOTE: Common Lisp, Scheme, Racket are in lang-lisp.el
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. RUBY
;; ============================================================================

(use-package ruby-mode
  :straight (:type built-in)
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode))
  :interpreter "ruby"
  :hook (ruby-mode . subword-mode))

(use-package ruby-ts-mode
  :straight (:type built-in)
  :mode "\\.rb\\'"
  :hook (ruby-ts-mode . subword-mode))

(use-package inf-ruby
  :after ruby-mode
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (ruby-ts-mode . inf-ruby-minor-mode)))

(use-package rspec-mode
  :after ruby-mode
  :hook ((ruby-mode . rspec-mode)
         (ruby-ts-mode . rspec-mode)))

(use-package bundler
  :after ruby-mode)

(use-package rubocop
  :after ruby-mode
  :hook ((ruby-mode . rubocop-mode)
         (ruby-ts-mode . rubocop-mode)))

;; ============================================================================
;; 2. HASKELL
;; ============================================================================

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . subword-mode)))

;; ============================================================================
;; 3. LUA
;; ============================================================================

(use-package lua-mode
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level 2))

;; ============================================================================
;; 4. SHELL / BASH
;; ============================================================================

(use-package sh-script
  :straight (:type built-in)
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.*rc\\'" . sh-mode)
         ("\\.profile\\'" . sh-mode)
         ("PKGBUILD\\'" . sh-mode))
  :hook (sh-mode . subword-mode)
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(use-package bash-ts-mode
  :straight (:type built-in)
  :mode "\\.sh\\'"
  :hook (bash-ts-mode . subword-mode))

(use-package shfmt
  :hook ((sh-mode . shfmt-on-save-mode)
         (bash-ts-mode . shfmt-on-save-mode))
  :custom
  (shfmt-arguments '("-i" "2" "-ci")))

;; ============================================================================
;; 5. SQL
;; ============================================================================

(use-package sql
  :straight (:type built-in)
  :mode ("\\.sql\\'" . sql-mode)
  :custom
  (sql-indent-offset 2))

(use-package sql-indent
  :after sql
  :hook (sql-mode . sqlind-minor-mode))

;; ============================================================================
;; 6. PROTOCOL BUFFERS
;; ============================================================================

(use-package protobuf-mode
  :mode "\\.proto\\'")

;; ============================================================================
;; 7. CMAKE
;; ============================================================================

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ============================================================================
;; 8. MAKEFILES
;; ============================================================================

(use-package make-mode
  :straight (:type built-in)
  :mode (("Makefile" . makefile-mode)
         ("\\.mk\\'" . makefile-mode)
         ("GNUmakefile" . makefile-mode)))

;; ============================================================================
;; 9. JUST (Command Runner)
;; ============================================================================

(use-package just-mode
  :mode ("justfile\\'" "\\.just\\'"))

(use-package justl
  :after just-mode
  :bind (:map just-mode-map
              ("C-c C-r" . justl)))

;; ============================================================================
;; 10. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Ruby
  (setf (alist-get 'rubocop apheleia-formatters)
        '("rubocop" "--auto-correct" "--stdin" filepath "--stderr"))
  (setf (alist-get 'ruby-mode apheleia-mode-alist) '(rubocop))
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) '(rubocop))

  ;; Shell
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) '(shfmt))

  ;; SQL
  (setf (alist-get 'sql-formatter apheleia-formatters)
        '("sql-formatter"))
  (setf (alist-get 'sql-mode apheleia-mode-alist) '(sql-formatter))

  ;; Lua
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "-"))
  (setf (alist-get 'lua-mode apheleia-mode-alist) '(stylua))

  ;; Haskell
  (setf (alist-get 'ormolu apheleia-formatters)
        '("ormolu" "--stdin-input-file" filepath))
  (setf (alist-get 'haskell-mode apheleia-mode-alist) '(ormolu)))

(provide 'lang-misc)
;;; lang-misc.el ends here
