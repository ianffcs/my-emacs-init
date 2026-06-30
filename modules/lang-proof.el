;;; lang-proof.el --- Proof Assistants and Formal Methods -*- lexical-binding: t; -*-

;;; Commentary:
;; Agda, Idris 2, Lean 4, TLA+

;;; Code:

(defvar eglot-server-programs)
(defvar lean4-rootdir)
(defvar lsp-clients)
(defvar lsp-enable-snippet)
(defvar lsp-headerline-breadcrumb-enable)
(defvar lsp-lens-enable)
(defvar lsp-modeline-code-actions-enable)
(defvar lsp-modeline-diagnostics-enable)
(defvar lsp-signature-auto-activate)
(declare-function lsp--client-request-handlers "lsp-mode")

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

(straight-use-package
 '(lean4-mode :type git :host github :repo "leanprover/lean4-mode"
              :files ("*.el" "data")))

;; lean4-mode's goal/info UI calls Lean-specific lsp-mode extensions such as
;; $/lean/plainGoal, so Lean intentionally uses lsp-mode instead of Eglot.
(with-eval-after-load 'lsp-mode
  (setq lsp-enable-snippet nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate nil
        lsp-lens-enable nil))

(when (file-executable-p (expand-file-name "~/.elan/bin/lean"))
  (setq lean4-rootdir (expand-file-name "~/.elan")))

(defun ian/lean4-ignore-inlay-hint-refresh (&rest _)
  "Acknowledge Lean's inlay hint refresh request without warning."
  nil)

(defun ian/lean4-register-lsp-request-handlers ()
  "Install Lean-specific lsp-mode request handlers."
  (when-let ((client (and (boundp 'lsp-clients)
                          (gethash 'lean4-lsp lsp-clients))))
    (puthash "workspace/inlayHint/refresh"
             #'ian/lean4-ignore-inlay-hint-refresh
             (lsp--client-request-handlers client))))

(with-eval-after-load 'lean4-mode
  (ian/lean4-register-lsp-request-handlers))

(add-to-list 'load-path
             (expand-file-name "straight/build/lean4-mode" user-emacs-directory))
(autoload 'lean4-mode "lean4-mode" "Major mode for Lean 4." t)
(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean4-mode))
(add-hook 'lean4-mode-hook #'subword-mode)
(add-hook 'lean4-mode-hook #'lsp)

;; ============================================================================
;; 4. TLA+
;; ============================================================================

(use-package tla-mode
  :straight t
  :mode "\\.tla\\'")

(provide 'lang-proof)
;;; lang-proof.el ends here
