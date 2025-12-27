;;; lang-lisp.el --- Lisp Family Languages -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for all Lisp-family languages:
;; - Clojure/ClojureScript (clojure-ts-mode preferred, CIDER)
;; - Common Lisp (Sly)
;; - Scheme (Geiser)
;; - Racket
;; - Emacs Lisp enhancements
;; - Fennel, Janet, Hy
;;
;; Structural editing: parinfer-rust (primary), paredit (secondary)

;;; Code:

;; ============================================================================
;; 1. STRUCTURAL EDITING - PARINFER-RUST (Primary)
;; ============================================================================

(use-package parinfer-rust-mode
  :straight (:host github :repo "justinbarclay/parinfer-rust-mode")
  :hook ((clojure-mode . parinfer-rust-mode)
         (clojure-ts-mode . parinfer-rust-mode)
         (clojurescript-mode . parinfer-rust-mode)
         (clojurec-mode . parinfer-rust-mode)
         (emacs-lisp-mode . parinfer-rust-mode)
         (lisp-mode . parinfer-rust-mode)
         (scheme-mode . parinfer-rust-mode)
         (racket-mode . parinfer-rust-mode)
         (fennel-mode . parinfer-rust-mode)
         (janet-mode . parinfer-rust-mode)
         (hy-mode . parinfer-rust-mode))
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-library-directory (expand-file-name "parinfer-rust" user-emacs-directory))
  ;; Automatically disable conflicting modes (electric-pair-mode, etc.) without prompting
  (parinfer-rust-auto-disable-troublesome-modes t)
  ;; Start in smart mode (adapts between indent and paren mode)
  (parinfer-rust-preferred-mode 'smart)
  :config
  ;; Keybinding to toggle parinfer mode
  (define-key parinfer-rust-mode-map (kbd "C-c C-p") #'parinfer-rust-toggle-paren-mode)
  (define-key parinfer-rust-mode-map (kbd "C-c C-s") #'parinfer-rust-switch-mode))

;; ============================================================================
;; 2. STRUCTURAL EDITING - PAREDIT (Secondary/Fallback)
;; ============================================================================

(use-package paredit
  :commands paredit-mode
  :config
  ;; Don't insert space before delimiters in certain contexts
  (add-to-list 'paredit-space-for-delimiter-predicates
               (lambda (endp delimiter)
                 (not (and (not endp)
                           (memq (char-before) '(?@ ?` ?' ?# ?~ ?^)))))))

(use-package highlight-parentheses
  :hook ((emacs-lisp-mode . highlight-parentheses-mode)
         (clojure-ts-mode . highlight-parentheses-mode)
         (lisp-mode . highlight-parentheses-mode)
         (scheme-mode . highlight-parentheses-mode)
         (cider-repl-mode . highlight-parentheses-mode)
         (sly-mrepl-mode . highlight-parentheses-mode)))

(use-package highlight-sexp
  :disabled  ; Enable if you want it
  :hook ((emacs-lisp-mode . highlight-sexp-mode)
         (clojure-ts-mode . highlight-sexp-mode)))

;; Toggle between parinfer and paredit
(defun ian/toggle-lisp-editing-mode ()
  "Toggle between parinfer-rust-mode and paredit-mode."
  (interactive)
  (if (bound-and-true-p parinfer-rust-mode)
      (progn
        (parinfer-rust-mode -1)
        (paredit-mode 1)
        (message "Switched to paredit"))
    (progn
      (paredit-mode -1)
      (parinfer-rust-mode 1)
      (message "Switched to parinfer"))))

(global-set-key (kbd "C-c (") #'ian/toggle-lisp-editing-mode)

;; ============================================================================
;; 3. CLOJURE - TREE-SITTER MODE (Preferred)
;; ============================================================================

(use-package clojure-ts-mode
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-clojurec-mode)
         ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
         ("\\.edn\\'" . clojure-ts-mode)
         ("\\.bb\\'" . clojure-ts-mode)     ; Babashka
         ("\\.cljd\\'" . clojure-ts-mode))  ; ClojureDart
  :hook ((clojure-ts-mode . subword-mode)
         (clojure-ts-clojurescript-mode . subword-mode)
         (clojure-ts-clojurec-mode . subword-mode))
  :custom
  (clojure-ts-indent-style 'fixed)
  (clojure-ts-toplevel-inside-comment-form t))

;; Keep clojure-mode as fallback (don't set :mode, let ts-mode take precedence)
(use-package clojure-mode
  :defer t
  :custom
  (clojure-indent-style 'always-align)
  (clojure-align-forms-automatically t)
  (clojure-toplevel-inside-comment-form t))

;; ============================================================================
;; 4. CIDER (Clojure Interactive Development)
;; ============================================================================

(use-package cider
  :after clojure-ts-mode
  :hook ((clojure-ts-mode . cider-mode)
         (clojure-ts-clojurescript-mode . cider-mode)
         (clojure-ts-clojurec-mode . cider-mode)
         (clojure-mode . cider-mode)
         (cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . parinfer-rust-mode))
  :bind (:map clojure-ts-mode-map
              ("C-c M-j" . cider-jack-in-clj)
              ("C-c M-J" . cider-jack-in-cljs)
              ("C-c M-c" . cider-connect-clj)
              ("C-c C-k" . cider-load-buffer)
              ("C-c C-z" . cider-switch-to-repl-buffer)
              ("C-c C-e" . cider-eval-last-sexp)
              ("C-c C-v" . cider-eval-defun-at-point)
              ("C-c C-r" . cider-eval-region)
              ("C-c C-b" . cider-eval-buffer)
              ("C-c C-n" . cider-repl-set-ns)
              ("C-c M-n" . cider-ns-refresh)
              ("C-c C-d d" . cider-doc)
              ("C-c C-d j" . cider-javadoc)
              ("C-c C-d c" . cider-clojuredocs)
              ("C-c M-i" . cider-inspect-last-result)
              ("C-c M-t v" . cider-toggle-trace-var)
              ("C-c M-t n" . cider-toggle-trace-ns))
  :custom
  ;; REPL Configuration
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-use-pretty-printing t)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-wrap-history t)
  (cider-repl-history-size 1000)
  (cider-repl-history-file (expand-file-name "cider-repl-history" user-emacs-directory))
  (cider-repl-prompt-function #'ian/cider-repl-prompt)
  ;; Eval Configuration
  (cider-show-error-buffer 'only-in-repl)
  (cider-auto-select-error-buffer nil)
  (cider-save-file-on-load t)
  (cider-eval-result-prefix "=> ")

  ;; Font-lock
  (cider-font-lock-dynamically '(macro core function var))

  ;; Eldoc
  (cider-eldoc-display-context-dependent-info t)

  ;; Debugging
  (cider-debug-prompt 'overlay)

  :config
  (defun ian/cider-repl-prompt (namespace)
    "Custom CIDER REPL prompt with NAMESPACE."
    (format "λ %s\n" namespace))
  (setq cider-use-overlays 'both)
  (setq nrepl-buffer-name-separator "-")
  (setq nrepl-buffer-name-show-port t)
  (setq cider-clojure-cli-global-options "-A:dev:test"))

(use-package cider-eval-sexp-fu
  :after cider)


;; ============================================================================
;; 5. CLJ-REFACTOR
;; ============================================================================

(use-package clj-refactor
  :after clojure-ts-mode
  :hook ((clojure-ts-mode . clj-refactor-mode)
         (clojure-ts-clojurescript-mode . clj-refactor-mode)
         (clojure-ts-clojurec-mode . clj-refactor-mode)
         (clojure-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-add-ns-to-blank-clj-files t)
  (cljr-favor-prefix-notation nil)
  (cljr-inject-dependencies-at-jack-in t)
  (cljr-warn-on-eval nil)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; ============================================================================
;; 6. BABASHKA (Clojure Scripting)
;; ============================================================================

(use-package babashka
  :straight (:host github :repo "licht1stein/babashka.el")
  :commands (babashka-run babashka-repl)
  :custom
  (babashka-command "bb"))

;; ============================================================================
;; 7. KAOCHA (Test Runner)
;; ============================================================================

(use-package kaocha-runner
  :after clojure-ts-mode
  :bind (:map clojure-ts-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

;; ============================================================================
;; 8. COMMON LISP (Sly)
;; ============================================================================

(use-package sly
  :commands sly
  :hook (lisp-mode . sly-editing-mode)
  :custom
  (inferior-lisp-program "sbcl")
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-net-coding-system 'utf-8-unix)
  :bind (:map lisp-mode-map
              ("C-c C-z" . sly-mrepl)
              ("C-c C-c" . sly-compile-defun)
              ("C-c C-k" . sly-compile-and-load-file)
              ("C-c C-l" . sly-load-file)))

(use-package sly-asdf
  :after sly)

(use-package sly-quicklisp
  :after sly)

(use-package sly-repl-ansi-color
  :after sly
  :config
  (push 'sly-repl-ansi-color sly-contribs))

;; ============================================================================
;; 9. SCHEME (Geiser)
;; ============================================================================

(use-package geiser
  :commands geiser
  :custom
  (geiser-active-implementations '(guile chicken chez mit racket))
  (geiser-default-implementation 'guile)
  (geiser-repl-use-other-window t))

(use-package geiser-guile
  :after geiser)

(use-package geiser-chicken
  :after geiser)

(use-package geiser-chez
  :after geiser)

(use-package geiser-mit
  :after geiser)

;; ============================================================================
;; 10. RACKET
;; ============================================================================

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook ((racket-mode . subword-mode)
         (racket-mode . racket-xp-mode))
  :bind (:map racket-mode-map
              ("C-c C-z" . racket-repl)
              ("C-c C-c" . racket-run)
              ("C-c C-k" . racket-run-and-switch-to-repl)
              ("C-c C-r" . racket-send-region)
              ("C-c C-e" . racket-send-last-sexp))
  :custom
  (racket-program "racket"))

;; ============================================================================
;; 11. EMACS LISP ENHANCEMENTS
;; ============================================================================

(use-package elisp-mode
  :straight (:type built-in)
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . (lambda () (setq-local fill-column 80)))))

;; Eros - Evaluation Result OverlayS (like CIDER for elisp)
(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :custom
  (eros-eval-result-prefix "=> "))

(use-package eval-sexp-fu
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

;; Better help for elisp - see core-utils.el (canonical location)
;; helpful bindings: C-h f/v/k/x, C-c C-d

;; Elisp demos/examples
(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Macrostep - expand macros inline
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; Package linting
(use-package package-lint
  :commands package-lint-current-buffer)

(use-package flycheck-package
  :after (flycheck package-lint)
  :config
  (flycheck-package-setup))

;; ============================================================================
;; 12. FENNEL (Lua Lisp)
;; ============================================================================

(use-package fennel-mode
  :mode "\\.fnl\\'"
  :hook (fennel-mode . subword-mode)
  :custom
  (fennel-program "fennel"))

;; ============================================================================
;; 13. JANET
;; ============================================================================

(use-package janet-mode
  :mode "\\.janet\\'"
  :hook (janet-mode . subword-mode))

;; ============================================================================
;; 14. HY (Python Lisp)
;; ============================================================================

;; ============================================================================
;; 15. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Clojure - cljfmt or zprint
  (setf (alist-get 'cljfmt apheleia-formatters)
        '("cljfmt" "fix" "-"))
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style :indent-only}"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) '(cljfmt))
  (setf (alist-get 'clojure-ts-mode apheleia-mode-alist) '(cljfmt))
  (setf (alist-get 'clojurescript-mode apheleia-mode-alist) '(cljfmt))
  (setf (alist-get 'clojurec-mode apheleia-mode-alist) '(cljfmt))

  ;; Racket
  (setf (alist-get 'raco-fmt apheleia-formatters)
        '("raco" "fmt"))
  (setf (alist-get 'racket-mode apheleia-mode-alist) '(raco-fmt))

  ;; Fennel
  (setf (alist-get 'fnlfmt apheleia-formatters)
        '("fnlfmt" "-"))
  (setf (alist-get 'fennel-mode apheleia-mode-alist) '(fnlfmt)))

;; ============================================================================
;; 17. ORG-BABEL
;; ============================================================================

(use-package ob-clojure
  :straight (:type built-in)
  :after org
  :custom
  (org-babel-clojure-backend 'cider))

;; ============================================================================
;; 18. RAINBOW DELIMITERS - REPL modes only (prog-mode hook in core-editor.el)
;; ============================================================================

;; Main programming modes are covered by prog-mode hook in core-editor.el
;; Here we add REPL modes which don't derive from prog-mode
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'sly-mrepl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode))

;; ============================================================================
;; 19. AGGRESSIVE-INDENT - See core-editor.el (canonical location)
;; ============================================================================

;; aggressive-indent configuration is in core-editor.el section 14
;; It covers: emacs-lisp-mode, clojure-mode, clojure-ts-mode,
;;            lisp-mode, scheme-mode, racket-mode

;; ============================================================================
;; 20. HELPER FUNCTIONS
;; ============================================================================

(defun ian/cider-jack-in-with-profile (profile)
  "Jack in with a specific PROFILE."
  (interactive "sProfile: ")
  (let ((cider-clojure-cli-global-options (format "-A:%s" profile)))
    (cider-jack-in-clj nil)))

(defun ian/cider-find-and-clear-repl-buffer ()
  "Find the REPL buffer and clear it."
  (interactive)
  (when-let ((repl-buffer (cider-current-repl)))
    (with-current-buffer repl-buffer
      (cider-repl-clear-buffer))))

(defun ian/lisp-describe-symbol-at-point ()
  "Describe symbol at point for current Lisp dialect."
  (interactive)
  (cond
   ((derived-mode-p 'clojure-mode 'clojure-ts-mode) (cider-doc))
   ((derived-mode-p 'emacs-lisp-mode) (helpful-at-point))
   ((derived-mode-p 'lisp-mode) (sly-describe-symbol (sly-symbol-at-point)))
   ((derived-mode-p 'scheme-mode) (geiser-doc-symbol-at-point))
   ((derived-mode-p 'racket-mode) (racket-xp-describe))
   (t (describe-symbol (symbol-at-point)))))

;;(global-set-key (kbd "C-c C-d C-d") #'ian/lisp-describe-symbol-at-point)

;; ============================================================================
;; 21. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/lisp-menu ()
                           "Lisp development commands"
                           ["Clojure/CIDER"
                            ("j" "Jack-in CLJ" cider-jack-in-clj)
                            ("J" "Jack-in CLJS" cider-jack-in-cljs)
                            ("c" "Connect" cider-connect-clj)
                            ("q" "Quit CIDER" cider-quit)]
                           ["Common Lisp"
                            ("s" "Sly" sly)
                            ("S" "Sly connect" sly-connect)]
                           ["Scheme"
                            ("g" "Geiser" geiser)
                            ("G" "Geiser connect" geiser-connect)]
                           ["Racket"
                            ("r" "Racket REPL" racket-repl)
                            ("R" "Racket run" racket-run)]
                           ["Editing"
                            ("(" "Toggle parinfer/paredit" ian/toggle-lisp-editing-mode)
                            ("p" "Parinfer mode" parinfer-rust-switch-mode)]
                           ["Eval"
                            ("e" "Eval last sexp" eval-last-sexp)
                            ("d" "Eval defun" eval-defun)
                            ("b" "Eval buffer" eval-buffer)])

  (dolist (map '(clojure-ts-mode-map
                 emacs-lisp-mode-map
                 lisp-mode-map))
    (when (boundp map)
      (define-key (symbol-value map) (kbd "C-c L") #'ian/lisp-menu))))

(provide 'lang-lisp)
;;; lang-lisp.el ends here
