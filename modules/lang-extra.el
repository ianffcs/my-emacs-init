;;; lang-extra.el --- Extra Languages -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for additional languages: Dart/Flutter, Hy, Forth, R, OpenSCAD.
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

;; Use eglot for Dart (consistent with the rest of this config).
;; lsp-dart pulled in lsp-mode which conflicts with eglot.
(with-eval-after-load 'dart-mode
  (add-hook 'dart-mode-hook #'eglot-ensure)
  (when (getenv "FLUTTER_HOME")
    (setq-default eglot-workspace-configuration
                  `(:dart (:flutterSdkPath ,(getenv "FLUTTER_HOME"))))))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-r" . flutter-run-or-hot-reload)))

;; ============================================================================
;; 2. HY (Python Lisp)
;; ============================================================================

(let ((ian/hy-bin (expand-file-name "private/venvs/hy/bin" user-emacs-directory)))
  (when (file-executable-p (expand-file-name "hy" ian/hy-bin))
    (add-to-list 'exec-path ian/hy-bin)
    (setenv "PATH" (concat ian/hy-bin path-separator (getenv "PATH")))))

(use-package hy-mode
  :mode "\\.hy\\'"
  :hook ((hy-mode . subword-mode)
         (hy-mode . smartparens-strict-mode)
         (hy-mode . rainbow-delimiters-mode))
  :bind (:map hy-mode-map
              ("C-c C-z" . run-hy)))

;; Org-babel support for Hy
(use-package ob-hy
  :after ob
  :if (executable-find "hy")
  :config
  (add-to-list 'org-babel-load-languages '(hy . t)))

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
;; 6. JULIA (Optional - via ESS or julia-mode)
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
