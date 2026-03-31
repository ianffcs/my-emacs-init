;;; lang-systems.el --- Systems Languages (C, C++, Rust, Go, Zig) -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for systems programming languages: C, C++, Rust, Go, Zig.
;; Tree-sitter modes are preferred.

;;; Code:

;; ============================================================================
;; 1. C / C++
;; ============================================================================

(use-package cc-mode
  :straight (:type built-in)
  :hook ((c-mode . subword-mode)
         (c++-mode . subword-mode))
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (c-mode . "linux")
                     (c++-mode . "stroustrup")
                     (other . "gnu")))
  (c-basic-offset 4))

(use-package c-ts-mode
  :straight (:type built-in)
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode))
  :hook (c-ts-mode . subword-mode)
  :custom
  (c-ts-mode-indent-offset 4))

(use-package c++-ts-mode
  :straight (:type built-in)
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hh\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode))
  :hook (c++-ts-mode . subword-mode)
  :custom
  (c-ts-mode-indent-offset 4))

;; ============================================================================
;; 2. RUST
;; ============================================================================

(use-package rust-mode
  :hook (rust-mode . subword-mode)
  :custom
  (rust-format-on-save nil)
  (rust-format-show-buffer nil))

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . subword-mode))

;; Cargo
(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)
         (rust-ts-mode . cargo-minor-mode))
  :bind (:map rust-mode-map
              ("C-c C-c c" . cargo-process-build)
              ("C-c C-c r" . cargo-process-run)
              ("C-c C-c t" . cargo-process-test)
              ("C-c C-c b" . cargo-process-bench)
              ("C-c C-c C" . cargo-process-clean)
              ("C-c C-c d" . cargo-process-doc)
              ("C-c C-c f" . cargo-process-fmt)
              ("C-c C-c k" . cargo-process-check)
              ("C-c C-c l" . cargo-process-clippy)
              :map rust-ts-mode-map
              ("C-c C-c c" . cargo-process-build)
              ("C-c C-c r" . cargo-process-run)
              ("C-c C-c t" . cargo-process-test)
              ("C-c C-c b" . cargo-process-bench)
              ("C-c C-c C" . cargo-process-clean)
              ("C-c C-c d" . cargo-process-doc)
              ("C-c C-c f" . cargo-process-fmt)
              ("C-c C-c k" . cargo-process-check)
              ("C-c C-c l" . cargo-process-clippy)))

;; ============================================================================
;; 3. GO
;; ============================================================================

(use-package go-mode
  :hook (go-mode . subword-mode)
  :custom
  (gofmt-command "goimports")
  (go-tag-args '("-transform" "camelcase")))

(use-package go-ts-mode
  :straight (:type built-in)
  :mode ("\\.go\\'" . go-ts-mode)
  :hook ((go-ts-mode . subword-mode)
         (go-ts-mode . (lambda ()
                         (add-hook 'before-save-hook #'gofmt-before-save nil t))))
  :custom
  (gofmt-command "goimports")
  (go-tag-args '("-transform" "camelcase")))

(use-package go-tag
  :commands (go-tag-add go-tag-remove)
  :bind (:map go-ts-mode-map
              ("C-c t a" . go-tag-add)
              ("C-c t r" . go-tag-remove)))

(use-package gotest
  :commands (go-test-current-test go-test-current-file go-test-current-project)
  :bind (:map go-ts-mode-map
              ("C-c t t" . go-test-current-test)
              ("C-c t f" . go-test-current-file)
              ("C-c t p" . go-test-current-project)
              ("C-c t c" . go-test-current-coverage)
              ("C-c t b" . go-test-current-benchmark)))

;; ============================================================================
;; 4. ZIG
;; ============================================================================

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode)
  :hook (zig-mode . subword-mode)
  :custom
  (zig-format-on-save nil))

;; ============================================================================
;; 5. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; C/C++ (clang-format)
  (setf (alist-get 'c-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) '(clang-format))

  ;; Rust (rustfmt)
  (setf (alist-get 'rust-mode apheleia-mode-alist) '(rustfmt))
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) '(rustfmt))

  ;; Go (goimports)
  (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports))

  ;; Zig
  (setf (alist-get 'zig-format apheleia-formatters)
        '("zig" "fmt" "--stdin"))
  (setf (alist-get 'zig-mode apheleia-mode-alist) '(zig-format)))

;; ============================================================================
;; 7. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/systems-menu ()
    "Systems programming commands"
    ["Rust/Cargo"
     ("c" "Build" cargo-process-build)
     ("r" "Run" cargo-process-run)
     ("t" "Test" cargo-process-test)
     ("k" "Check" cargo-process-check)
     ("l" "Clippy" cargo-process-clippy)
     ("f" "Format" cargo-process-fmt)]
    ["Go"
     ("g t" "Test" go-test-current-test)
     ("g f" "Test file" go-test-current-file)
     ("g p" "Test project" go-test-current-project)
     ("g i" "Imports" go-goto-imports)]
    ["Build"
     ("m" "Make" compile)
     ("M" "Recompile" recompile)])

  (with-eval-after-load 'rust-mode
    (define-key rust-mode-map (kbd "C-c C-m") #'ian/systems-menu))
  (with-eval-after-load 'rust-ts-mode
    (define-key rust-ts-mode-map (kbd "C-c C-m") #'ian/systems-menu))
  (with-eval-after-load 'go-ts-mode
    (define-key go-ts-mode-map (kbd "C-c C-m") #'ian/systems-menu)))

(provide 'lang-systems)
;;; lang-systems.el ends here
