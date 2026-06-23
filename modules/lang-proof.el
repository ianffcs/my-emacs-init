;;; lang-proof.el --- Proof Assistants and Formal Methods -*- lexical-binding: t; -*-

;;; Commentary:
;; Agda, Idris 2, Lean 4, TLA+

;;; Code:

;; ============================================================================
;; 1. AGDA (Dependently Typed)
;; ============================================================================

;; agda2-mode is bundled with the Agda compiler; load it from there.
(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; ============================================================================
;; 2. IDRIS 2
;; ============================================================================

(use-package idris2-mode
  :straight (:host github :repo "idris-community/idris2-mode")
  :mode ("\\.idr\\'" "\\.lidr\\'")
  :hook ((idris2-mode . subword-mode)
         (idris2-mode . eglot-ensure))
  :bind (:map idris2-mode-map
              ("C-c C-l" . idris2-load-file)
              ("C-c C-t" . idris2-type-at-point)
              ("C-c C-d" . idris2-add-clause)
              ("C-c C-s" . idris2-proof-search)
              ("C-c C-c" . idris2-case-split)
              ("C-c C-a" . idris2-make-cases-from-type)
              ("C-c C-r" . idris2-refine)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(idris2-mode . ("idris2" "--lsp"))))

;; ============================================================================
;; 3. LEAN 4
;; ============================================================================

(use-package lean4-mode
  :straight (:type git :host github :repo "leanprover/lean4-mode")
  :mode "\\.lean\\'"
  :hook (lean4-mode . subword-mode))

;; ============================================================================
;; 4. TLA+
;; ============================================================================

(use-package tla-mode
  :straight t
  :mode "\\.tla\\'")

(provide 'lang-proof)
;;; lang-proof.el ends here
