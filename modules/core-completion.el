;;; core-completion.el --- Completion Framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion framework: Vertico, Consult, Corfu, Embark, Orderless.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. VERTICO (Vertical Completion UI)
;; ============================================================================

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-insert)
              ("C-u" . vertico-scroll-down)
              ("C-d" . vertico-scroll-up)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-scroll-margin 2)
  :config
  ;; Prefix current candidate with arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

;; ============================================================================
;; 2. COMPANY (Dependency for company-* integrations)
;; ============================================================================

(use-package company
  :defer t
  :config
  (add-to-list 'company-backends 'company-ansible)
  (add-to-list 'company-backends 'company-nginx)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-latex-commands)
  (add-to-list 'company-backends 'company-emoji))

;; Vertico extensions
(use-package vertico-directory
  :straight nil
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :straight nil
  :after vertico
  :config
  (setq vertico-multiform-commands
        '((consult-line buffer)
          (consult-imenu buffer)
          (consult-outline buffer)
          (execute-extended-command flat)))
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)))
  (vertico-multiform-mode 1))

(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("C-q" . vertico-quick-exit)))

;; ============================================================================
;; 2. ORDERLESS (Completion Style)
;; ============================================================================

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp)))

;; ============================================================================
;; 3. MARGINALIA (Rich Annotations)
;; ============================================================================

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil)))

;; ============================================================================
;; 4. CONSULT (Search & Navigation)
;; ============================================================================

(use-package consult
  :bind (;; C-c bindings
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ;; C-x bindings
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; M-g bindings (goto)
         ("M-g f" . consult-flymake)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-preview-key '(:debounce 0.2 any))
  (consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  :config
  ;; Configure preview
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  ;; Use Projectile for project root
  (setq consult-project-function
        (lambda (_)
          (cond
           ((fboundp 'projectile-project-root) (projectile-project-root))
           ((fboundp 'project-root)
            (when-let ((p (project-current nil)))
              (car (project-roots p))))
           (t default-directory)))))

;; Consult + Flycheck
(use-package consult-flycheck
  :disabled t
  ;; NOTE: This config uses Flymake (via Eglot). Enable Flycheck if you want this.
  :after (consult flycheck))

;; Consult + Projectile
(use-package consult-projectile
  :after (consult projectile))

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; ============================================================================
;; 5. EMBARK (Actions on Targets)
;; ============================================================================

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; 6. CORFU (In-buffer Completion)
;; ============================================================================

(use-package corfu
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-l" . corfu-insert)
              ("RET" . corfu-insert)
              ("C-g" . corfu-quit)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Corfu terminal support
(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode +1))

;; ============================================================================
;; 7. CAPE (Completion At Point Extensions)
;; ============================================================================

(use-package cape
  :bind (("C-c C-p p" . completion-at-point)
         ("C-c C-p t" . complete-tag)
         ("C-c C-p d" . cape-dabbrev)
         ("C-c C-p h" . cape-history)
         ("C-c C-p f" . cape-file)
         ("C-c C-p k" . cape-keyword)
         ("C-c C-p s" . cape-elisp-symbol)
         ("C-c C-p e" . cape-elisp-block)
         ("C-c C-p a" . cape-abbrev)
         ("C-c C-p l" . cape-line)
         ("C-c C-p w" . cape-dict))
  :init
  ;; Add to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; Silence pcomplete capf (no messages)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Make pcomplete completion non-blocking
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-noninterruptible))

;; ============================================================================
;; 8. KIND-ICON (Icons for Completion)
;; ============================================================================

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ============================================================================
;; 9. PRESCIENT (Frecency Sorting) - Alternative
;; ============================================================================

(use-package prescient
  :disabled  ; Using orderless instead
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :disabled
  :after (vertico prescient)
  :config
  (vertico-prescient-mode 1))

(use-package corfu-prescient
  :disabled
  :after (corfu prescient)
  :config
  (corfu-prescient-mode 1))

;; ============================================================================
;; 10. COMPLETION TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/completion-menu ()
                           "Completion commands"
                           ["Search"
                            ("l" "Line" consult-line)
                            ("L" "Line multi" consult-line-multi)
                            ("r" "Ripgrep" consult-ripgrep)
                            ("g" "Grep" consult-grep)
                            ("f" "Find file" consult-find)]
                           ["Navigate"
                            ("i" "Imenu" consult-imenu)
                            ("I" "Imenu multi" consult-imenu-multi)
                            ("o" "Outline" consult-outline)
                            ("m" "Mark" consult-mark)
                            ("M" "Global mark" consult-global-mark)]
                           ["Buffers"
                            ("b" "Buffer" consult-buffer)
                            ("B" "Buffer other" consult-buffer-other-window)
                            ("p" "Project buffer" consult-project-buffer)]
                           ["Cape"
                            ("c d" "Dabbrev" cape-dabbrev)
                            ("c f" "File" cape-file)
                            ("c k" "Keyword" cape-keyword)
                            ("c s" "Symbol" cape-elisp-symbol)])

  (global-set-key (kbd "C-c s") #'ian/completion-menu))

(provide 'core-completion)
;;; core-completion.el ends here
