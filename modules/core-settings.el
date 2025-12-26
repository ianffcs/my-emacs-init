;;; core-settings.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Fundamental Emacs configuration: performance, encoding, backups, defaults.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. PERFORMANCE (Early)
;; ============================================================================


;; Disable bidirectional text for performance
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; ============================================================================
;; 2. ENCODING
;; ============================================================================

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ============================================================================
;; 4. HISTORY & PERSISTENCE
;; ============================================================================

;; Savehist - minibuffer history
(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (savehist-autosave-interval 60)
  (history-length 1000)
  (history-delete-duplicates t)
  :config
  (savehist-mode 1))

;; Recentf - recent files
(use-package recentf
  :straight (:type built-in)
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude
               (expand-file-name "straight/build/" user-emacs-directory))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "/tmp/")
  (add-to-list 'recentf-exclude "/ssh:")
  (add-to-list 'recentf-exclude "\\.git/"))

;; Save place - remember cursor position
(use-package saveplace
  :straight (:type built-in)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (save-place-mode 1))

;; Desktop - save session (disabled by default)
(use-package desktop
  :straight (:type built-in)
  :disabled
  :custom
  (desktop-dirname (expand-file-name "desktop" user-emacs-directory))
  (desktop-base-file-name "desktop")
  (desktop-restore-eager 5)
  (desktop-save 'ask-if-new))

;; ============================================================================
;; 5. BASIC UI SETTINGS
;; ============================================================================

;; Disable GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Startup settings
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; Frame settings
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b - Emacs"))

;; Cursor and display
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)

;; Fringe
(set-fringe-mode '(8 . 8))

;; Line numbers
(setq display-line-numbers-type 'relative
      display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; ============================================================================
;; 6. EDITING DEFAULTS
;; ============================================================================

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              standard-indent 2
              fill-column 80)

;; Scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Matching parentheses
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Electric pairs - prefer parinfer
;; (electric-pair-mode 1)
;;(setq electric-pair-preserve-balance t
;;      electric-pair-delete-adjacent-pairs t)

;; Delete selection when typing
(delete-selection-mode 1)

;; Sentence settings
(setq sentence-end-double-space nil)

;; Final newline
(setq require-final-newline t)

;; ============================================================================
;; 7. FILE HANDLING
;; ============================================================================

;; Don't ask for confirmation when following symlinks
(setq vc-follow-symlinks t)

;; Don't create lock files
(setq create-lockfiles nil)

;; Refresh buffers when files change
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Large file warning
(setq large-file-warning-threshold (* 100 1024 1024))  ; 100MB

;; So-long mode for files with long lines
(global-so-long-mode 1)

;; ============================================================================
;; 8. MACOS SPECIFIC
;; ============================================================================

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-control-modifier 'control
        mac-right-option-modifier 'none
        ns-use-native-fullscreen t
        ns-pop-up-frames nil)

  ;; Smooth scrolling
  (setq mac-mouse-wheel-smooth-scroll t))

;; ============================================================================
;; 9. CUSTOM FILE
;; ============================================================================

(setq custom-file (expand-file-name "custom/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; ============================================================================
;; 10. NATIVE COMPILATION
;; ============================================================================

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t
        native-compile-prune-cache t))

;; ============================================================================
;; 11. MISC SETTINGS
;; ============================================================================

;; Yes/No prompts
(setq use-short-answers t)

;; Disable bell
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Kill process buffers without confirmation
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Enable disabled commands
(setq disabled-command-function nil)

;; Recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Increase undo limit
(setq undo-limit (* 4 1024 1024)        ; 4MB
      undo-strong-limit (* 6 1024 1024) ; 6MB
      undo-outer-limit (* 12 1024 1024)) ; 12MB

;; Word wrap at word boundaries
(setq-default word-wrap t)

;; Truncate lines by default in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Column indicator
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(provide 'core-settings)
;;; core-settings.el ends here
