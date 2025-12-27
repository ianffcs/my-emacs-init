;;; lang-extra.el --- Extra Languages -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for additional languages: Dart/Flutter, Hy, Forth, R, OpenSCAD, Agda.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. DART / FLUTTER
;; ============================================================================

(use-package dart-mode
  :mode "\\.dart\\'"
  :hook (dart-mode . subword-mode)
  :bind (:map dart-mode-map
              ("C-c C-o" . dart-format-buffer))
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))

(use-package dart-server
  :after dart-mode)

(use-package lsp-dart
  :after (dart-mode lsp-mode)
  :hook (dart-mode . lsp)
  :custom
  (lsp-dart-flutter-sdk-dir (getenv "FLUTTER_HOME"))
  :config
  ;; Optional: Debug template for Flutter
  ;; (dap-register-debug-template "Flutter :: Custom debug"
  ;;   (list :flutterPlatform "x86_64"
  ;;         :program "lib/main_debug.dart"
  ;;         :args '("--flavor" "customer_a")))
  )

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-r" . flutter-run-or-hot-reload)))

;; ============================================================================
;; 2. HY (Python Lisp)
;; ============================================================================

(use-package hy-mode
  :mode "\\.hy\\'"
  :hook ((hy-mode . subword-mode)
         (hy-mode . smartparens-strict-mode)
         (hy-mode . rainbow-delimiters-mode))
  :bind (:map hy-mode-map
              ("C-c C-z" . run-hy)))

;; Org-babel support for Hy
(use-package ob-hy
  :after org)

;; ============================================================================
;; 3. FORTH
;; ============================================================================

(use-package forth-mode
  :mode ("\\.f\\'" "\\.fs\\'" "\\.fth\\'" "\\.4th\\'")
  :bind (:map forth-mode-map
              ("C-x C-e" . forth-eval-last-expression)
              ("C-c C-c" . forth-eval-region)))

;; ============================================================================
;; 4. R / ESS (Emacs Speaks Statistics)
;; ============================================================================

(use-package ess
  :mode (("\\.R\\'" . ess-r-mode)
         ("\\.r\\'" . ess-r-mode)
         ("\\.Rmd\\'" . ess-r-mode))
  :hook (ess-r-mode . subword-mode)
  :custom
  (ess-style 'RStudio)
  (ess-use-flymake nil)
  (ess-eval-visibly 'nowait)
  (ess-ask-for-ess-directory nil)
  :config)

;; ============================================================================
;; 5. OPENSCAD (3D Modeling)
;; ============================================================================

(use-package scad-mode
  :mode "\\.scad\\'"
  :custom
  (scad-command "openscad"))

(use-package scad-preview
  :after scad-mode
  :bind (:map scad-mode-map
              ("C-c C-p" . scad-preview-mode)))

;; ============================================================================
;; 6. AGDA (Dependently Typed Language)
;; ============================================================================

;; Agda-mode is typically installed via agda itself:
;; Run: agda-mode setup
;; This will add the following to your init:

(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; If agda-mode is installed manually:
(use-package agda2-mode
  :disabled  ; Enable if you have agda-mode installed
  :mode "\\.agda\\'"
  :custom
  (agda2-highlight-face-groups 'default-faces))

;; ============================================================================
;; 7. JULIA (Optional - via ESS or julia-mode)
;; ============================================================================

(use-package julia-mode
  :mode "\\.jl\\'"
  :hook (julia-mode . subword-mode))

(use-package julia-repl
  :after julia-mode
  :hook (julia-mode . julia-repl-mode)
  :bind (:map julia-mode-map
              ("C-c C-z" . julia-repl)
              ("C-c C-c" . julia-repl-send-region-or-line)))

;; ============================================================================
;; 8. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Dart
  (setf (alist-get 'dart-format apheleia-formatters)
        '("dart" "format"))
  (setf (alist-get 'dart-mode apheleia-mode-alist) '(dart-format))

  ;; R (styler)
  (setf (alist-get 'styler apheleia-formatters)
        '("Rscript" "-e" "styler::style_text(readLines('stdin'))"))
  (setf (alist-get 'ess-r-mode apheleia-mode-alist) '(styler)))

(provide 'lang-extra)
;;; lang-extra.el ends here
