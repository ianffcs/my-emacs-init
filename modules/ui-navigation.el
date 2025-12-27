;;; ui-navigation.el --- Navigation & Search -*- lexical-binding: t; -*-

;;; Commentary:
;; Navigation tools: Avy, search, jumping, bookmarks.
;; NOTE: ace-window moved to ui-windows.el
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. AVY (Jump to visible text)
;; ============================================================================

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("C-:" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c j j" . avy-goto-char-timer)
         ("C-c j l" . avy-goto-line)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j c" . avy-goto-char)
         ("C-c j C" . avy-goto-char-2))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-background t)
  (avy-all-windows t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

;; Link avy with embark
(use-package avy-embark-collect
  :after (avy embark)
  :config
  (setf (alist-get 'avy embark-collect-initial-view-alist) 'list))

;; ============================================================================
;; 2. GOTO-CHANGER (Jump to last change)
;; ============================================================================

(use-package goto-chg
  :bind (("C-c j ," . goto-last-change)
         ("C-c j ." . goto-last-change-reverse)))

;; ============================================================================
;; 3. IMENU (Jump to definitions)
;; ============================================================================

(use-package imenu
  :straight (:type built-in)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 100))

(use-package imenu-list
  :bind ("C-c j i" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-position 'right))

;; ============================================================================
;; 4. BOOKMARKS (bm)
;; ============================================================================

(use-package bm
  :bind (("C-c j b" . bm-toggle)
         ("C-c j n" . bm-next)
         ("C-c j p" . bm-previous)
         ("C-c j B" . bm-show-all))
  :custom
  (bm-cycle-all-buffers t)
  (bm-highlight-style 'bm-highlight-only-fringe))

;; ============================================================================
;; 5. PULSE (Highlight current line after jump)
;; ============================================================================

(use-package pulse
  :straight (:type built-in)
  :custom
  (pulse-delay 0.04)
  (pulse-iterations 10)
  :config
  (defun ian/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  ;; Pulse after jump commands
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     recenter-top-bottom
                     other-window
                     avy-goto-char-timer
                     avy-goto-line
                     consult-line
                     consult-goto-line
                     imenu
                     xref-find-definitions
                     xref-find-references))
    (advice-add command :after #'ian/pulse-line)))

;; ============================================================================
;; 6. WGREP (Editable grep buffers)
;; ============================================================================

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package pcre2el
  :config
  (pcre-mode))

;; ============================================================================
;; 7. DEADGREP (Fast ripgrep interface)
;; ============================================================================

(use-package deadgrep
  :bind ("C-c s d" . deadgrep))

;; ============================================================================
;; 8. RG (Ripgrep)
;; ============================================================================

(use-package rg
  :bind ("C-c s r" . rg-menu)
  :config
  (rg-enable-default-bindings))

;; ============================================================================
;; 9. NAVIGATION TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/jump-menu ()
    "Jump and navigation commands"
    ["Avy"
     ("j" "Char timer" avy-goto-char-timer)
     ("c" "Char" avy-goto-char)
     ("C" "Char 2" avy-goto-char-2)
     ("w" "Word" avy-goto-word-1)
     ("l" "Line" avy-goto-line)]
    ["Change"
     ("," "Last change" goto-last-change)
     ("." "Next change" goto-last-change-reverse)]
    ["Bookmarks"
     ("b" "Toggle BM" bm-toggle)
     ("n" "Next BM" bm-next)
     ("p" "Prev BM" bm-previous)
     ("B" "Show all BM" bm-show-all)]
    ["Other"
     ("i" "Imenu" imenu)
     ("I" "Imenu list" imenu-list-smart-toggle)])

  (global-set-key (kbd "C-c j") #'ian/jump-menu))

(provide 'ui-navigation)
;;; ui-navigation.el ends here
