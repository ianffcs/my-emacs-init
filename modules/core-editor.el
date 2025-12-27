;;; core-editor.el --- Editor Behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Editor configuration: indentation, parentheses, undo, whitespace, etc.
;; This is the canonical location for editing packages used across modes.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. INDENTATION
;; ============================================================================

(setq-default indent-tabs-mode nil
              tab-width 4
              standard-indent 2
              fill-column 80)

;; Tab always indents
(setq tab-always-indent 'complete)

;; Electric indent
(electric-indent-mode 1)

;; ============================================================================
;; 2. PARENTHESES & PAIRS
;; ============================================================================

;; Show matching parentheses
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'parenthesis)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Electric pairs
(use-package elec-pair
  :straight (:type built-in)
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-skip-whitespace nil)
  (electric-pair-open-newline-between-pairs t)
  :config
  ;; Disable electric-pair in minibuffer
  (defun ian/inhibit-electric-pair ()
    (setq-local electric-pair-mode nil))
  (add-hook 'minibuffer-setup-hook #'ian/inhibit-electric-pair))

;; Rainbow delimiters - CANONICAL LOCATION
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
;; 3. UNDO / REDO
;; ============================================================================

;; Increase undo limit
(setq undo-limit (* 4 1024 1024)         ; 4MB
      undo-strong-limit (* 6 1024 1024)  ; 6MB
      undo-outer-limit (* 12 1024 1024)) ; 12MB

;; Vundo (visual undo tree)
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  (vundo-window-max-height 8))

;; Undo-fu (better undo/redo)
(use-package undo-fu
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)
         ("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

;; Persist undo history
(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))

;; ============================================================================
;; 4. WHITESPACE
;; ============================================================================

;; Cleanup whitespace on save
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Visualize whitespace
(use-package whitespace
  :straight (:type built-in)
  :diminish
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)
         (conf-mode . whitespace-mode))
  :custom
  (whitespace-line-column 120)
  :config
  ;; Don't show spaces, just problematic whitespace
  (setq whitespace-style '(face trailing tabs empty)))

;; Delete trailing whitespace
(use-package ws-butler
  :diminish
  :hook ((prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)))

;; ============================================================================
;; 5. LINE NUMBERS
;; ============================================================================

(setq display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Disable in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ============================================================================
;; 6. FILL COLUMN
;; ============================================================================

(setq-default fill-column 80)

;; Visual fill column indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Visual fill column (for writing)
(use-package visual-fill-column
  :hook ((org-mode . visual-fill-column-mode)
         (markdown-mode . visual-fill-column-mode))
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

;; ============================================================================
;; 7. TEXT MANIPULATION - CANONICAL LOCATION
;; ============================================================================

;; Delete selection when typing
(delete-selection-mode 1)

;; Subword mode (CamelCase navigation)
(global-subword-mode 1)

;; String inflection (change case style)
(use-package string-inflection
  :bind ("C-c q c" . string-inflection-all-cycle))

;; Expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Edit multiple occurrences simultaneously
(use-package iedit
  :bind ("C-c ;" . iedit-mode))

;; Move text up/down
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;; Smart shift (indent regions)
(use-package smart-shift
  :config
  (global-smart-shift-mode t))

(use-package zzz-to-char
  :bind ("M-z" . zzz-up-to-char))

(defun ian/unfill-paragraph (&optional region)
  "Make a multi-line paragraph into a single line."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun ian/unfill-region (beg end)
  "Unfill the region, joining paragraphs into single lines."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(global-set-key (kbd "M-Q") #'ian/unfill-paragraph)

;; ============================================================================
;; 8. COMMENTING
;; ============================================================================

;; Better commenting
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; ============================================================================
;; 9. SCROLLING
;; ============================================================================

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Pixel scroll (smooth scrolling)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; ============================================================================
;; 10. WRAPPING
;; ============================================================================

;; Word wrap at word boundaries
(setq-default word-wrap t)

;; Truncate lines in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Visual line mode for text
(add-hook 'text-mode-hook #'visual-line-mode)

;; ============================================================================
;; 11. SENTENCE & PARAGRAPH
;; ============================================================================

(setq sentence-end-double-space nil)
(setq paragraph-start "\f\\|[ \t]*$"
      paragraph-separate "[ \t\f]*$")

;; ============================================================================
;; 12. FINAL NEWLINE
;; ============================================================================

(setq require-final-newline t)
(setq mode-require-final-newline t)

;; ============================================================================
;; 13. HIGHLIGHT
;; ============================================================================

(use-package volatile-highlights
  :diminish
  :config
  (volatile-highlights-mode t))

(use-package highlight-symbol
  :diminish
  :hook ((prog-mode . highlight-symbol-mode)
         (highlight-symbol-mode . highlight-symbol-nav-mode))
  :custom
  (highlight-symbol-idle-delay 0.25)
  (highlight-symbol-on-navigation-p t)
  (highlight-symbol-highlight-single-occurrence nil))

;; Highlight TODO keywords
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#FF0000")
     ("FIXME" . "#FF0000")
     ("DEBUG" . "#A020F0")
     ("HACK" . "#FFA500")
     ("NOTE" . "#1E90FF")
     ("REVIEW" . "#1E90FF")
     ("XXX" . "#FF4500")
     ("DEPRECATED" . "#808080"))))

;; Highlight numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Highlight escape sequences
(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; ============================================================================
;; 14. AGGRESSIVE INDENT - CANONICAL LOCATION
;; ============================================================================

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (clojure-ts-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)
         (racket-mode . aggressive-indent-mode)))

;; ============================================================================
;; 15. EDITORCONFIG - CANONICAL LOCATION
;; ============================================================================

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

;; ============================================================================
;; 16. SO-LONG (Handle files with long lines)
;; ============================================================================

(global-so-long-mode 1)

;; ============================================================================
;; 17. AUTOREVERT
;; ============================================================================

(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode
  :custom
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

;; ============================================================================
;; 18. BOOKMARKS
;; ============================================================================

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)))

(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

;; ============================================================================
;; 19. REGISTERS
;; ============================================================================

(use-package register
  :straight (:type built-in)
  :config
  ;; Useful registers
  (set-register ?i (cons 'file user-init-file))
  (set-register ?o (cons 'file (expand-file-name "~/org")))
  (set-register ?s (cons 'file (expand-file-name "~/src"))))

(defun ian/clear-registers ()
  "Remove all saved registers."
  (interactive)
  (setq register-alist nil)
  (message "All registers cleared"))

;; Add more register shortcuts
(set-register ?t (cons 'file (expand-file-name "~/org/todo.org")))
(set-register ?c (cons 'file (expand-file-name "docs/cheatsheet.org" user-emacs-directory)))

;; ============================================================================
;; 20. HELPER FUNCTIONS
;; ============================================================================

(defun ian/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun ian/untabify-buffer ()
  "Convert tabs to spaces in buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun ian/cleanup-buffer ()
  "Cleanup buffer: indent, untabify, remove trailing whitespace."
  (interactive)
  (ian/indent-buffer)
  (ian/untabify-buffer)
  (whitespace-cleanup)
  (message "Buffer cleaned up"))

(defun ian/duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)))

(defun ian/open-line-above ()
  "Open a new line above the current one."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun ian/open-line-below ()
  "Open a new line below the current one."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

;; Keybindings
(global-set-key (kbd "C-c e i") #'ian/indent-buffer)
(global-set-key (kbd "C-c e c") #'ian/cleanup-buffer)
(global-set-key (kbd "C-c e d") #'ian/duplicate-line)
(global-set-key (kbd "C-S-<return>") #'ian/open-line-above)
(global-set-key (kbd "S-<return>") #'ian/open-line-below)

(provide 'core-editor)
;;; core-editor.el ends here
