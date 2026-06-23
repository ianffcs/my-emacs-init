;;; ui-windows.el --- Window and workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window management, workspaces, and frame handling.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. WINNER MODE (Undo Window Changes)
;; ============================================================================

(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys t))

;; ============================================================================
;; 2. WINDMOVE (Arrow Key Window Navigation)
;; ============================================================================

(use-package windmove
  :straight (:type built-in)
  :demand t
  :config
  (windmove-default-keybindings 'super)
  (windmove-swap-states-default-keybindings '(super shift)))

;; ============================================================================
;; 3. ACE-WINDOW (Quick Window Switching)
;; ============================================================================

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-background t)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?x aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?b aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?e aw-execute-command-other-window "Execute Command Other Window")
     (?F aw-split-window-fair "Split Fair Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?h aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?? aw-show-dispatch-help)))
  :config
  ;; Set face for ace-window leading char
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "red"
                      :weight 'bold
                      :height 2.0))

;; ============================================================================
;; 4. TAB-BAR (Workspaces)
;; ============================================================================

(use-package tab-bar
  :straight (:type built-in)
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-show 1
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-new-tab-to 'rightmost
        tab-bar-tab-hints t
        tab-bar-select-tab-modifiers '(super)
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  ;; Custom tab bar appearance
  (setq tab-bar-tab-name-function
        (lambda ()
          (let ((name (buffer-name)))
            (if (> (length name) 20)
                (concat (substring name 0 17) "...")
              name)))))

;; Tab-bar history (per-tab history)
(use-package tab-bar-history
  :straight (:type built-in)
  :after tab-bar
  :config
  (tab-bar-history-mode 1))

;; ============================================================================
;; 5. TRANSPOSE-FRAME
;; ============================================================================

(use-package transpose-frame
  :bind (("C-c w t" . transpose-frame)
         ("C-c w f" . flip-frame)
         ("C-c w F" . flop-frame)
         ("C-c w R" . rotate-frame-clockwise)))

;; ============================================================================
;; 6. GOLDEN-RATIO (Auto-resize Windows)
;; ============================================================================

(use-package golden-ratio
  :disabled  ; Enable if you want auto-resizing
  :diminish
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-modes '(treemacs-mode))
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

;; ============================================================================
;; 7. POPPER (Popup Management)
;; ============================================================================

(use-package popper
  :hook (after-init . popper-mode)
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*Backtrace\\*"
     "\\*Compile-Log\\*"
     help-mode
     helpful-mode
     compilation-mode
     flymake-diagnostics-buffer-mode)))

;; ============================================================================
;; 8. SHACKLE (Window Rule Management)
;; ============================================================================

;; shackle disabled: conflicts with popper which also manages display-buffer rules.
;; popper (section 7) handles popup placement instead.

;; ============================================================================
;; 9. WORKSPACE HUD
;; ============================================================================

(use-package emacs-egui
  :straight (emacs-egui
             :type git
             :host github
             :repo "nohzafk/emacs-egui"
             :files ("lisp/*.el")))

(use-package workspace-hud
  :straight (workspace-hud
             :type git
             :host github
             :repo "nohzafk/emacs-workspace-hud"
             :local-repo "emacs-workspace-hud"
             :files ("lisp/*.el" "ui" "emacs-egui")
             :pre-build ("git" "submodule" "update" "--init" "--recursive")
             :post-build ("sh" "-c" "if command -v wasm-pack >/dev/null 2>&1; then cd ui && wasm-pack build --target web --release; else echo 'workspace-hud: wasm-pack not found; install it and run M-x straight-rebuild-package RET workspace-hud RET'; fi"))
  :commands (workspace-hud-toggle
             workspace-hud-show
             workspace-hud-hide
             workspace-hud-refresh
             workspace-hud-cleanup
             workspace-hud-auto-mode)
  :bind ("C-c w H" . workspace-hud-toggle)
  :custom
  (workspace-hud-width 260)
  (workspace-hud-margin-right 19)
  (workspace-hud-margin-top 20)
  :init
  ;; Keep this manual for now. Auto mode currently errors in some buffers when
  ;; the package cannot resolve a Git root.
  (setq workspace-hud-auto-mode nil))

;; ============================================================================
;; 10. WINDOW HELPER FUNCTIONS
;; ============================================================================

(defun ian/split-window-right-and-focus ()
  "Split window right and move to the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun ian/split-window-below-and-focus ()
  "Split window below and move to the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun ian/delete-window-or-frame ()
  "Delete window, or delete frame if only one window."
  (interactive)
  (if (= 1 (length (window-list)))
      (delete-frame)
    (delete-window)))

(defun ian/toggle-window-dedicated ()
  "Toggle whether the current window is dedicated."
  (interactive)
  (let ((dedicated (not (window-dedicated-p))))
    (set-window-dedicated-p (selected-window) dedicated)
    (message "Window %s dedicated"
             (if dedicated "is now" "is no longer"))))


;; Keybindings
(global-set-key (kbd "C-c w v") #'ian/split-window-right-and-focus)
(global-set-key (kbd "C-c w h") #'ian/split-window-below-and-focus)
(global-set-key (kbd "C-c w d") #'ian/delete-window-or-frame)
(global-set-key (kbd "C-c w D") #'ian/toggle-window-dedicated)
(global-set-key (kbd "C-c w =") #'balance-windows)
(global-set-key (kbd "C-c w m") #'ian/toggle-maximize-buffer)

;; ============================================================================
;; 11. WINDOW TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/window-menu ()
    "Window management commands"
    ["Navigation"
     ("o" "Other window" other-window)
     ("a" "Ace window" ace-window)
     ("0" "Delete window" delete-window)
     ("1" "Delete others" delete-other-windows)
     ("2" "Split below" split-window-below)
     ("3" "Split right" split-window-right)]
    ["Resize"
     ("=" "Balance" balance-windows)
     ("m" "Maximize" ian/toggle-maximize-buffer)
     ("+" "Enlarge" enlarge-window)
     ("-" "Shrink" shrink-window)]
    ["Layout"
     ("t" "Transpose" transpose-frame)
     ("f" "Flip" flip-frame)
     ("F" "Flop" flop-frame)
     ("R" "Rotate" rotate-frame-clockwise)]
    ["Tabs"
     ("T" "New tab" tab-bar-new-tab)
     ("W" "Close tab" tab-bar-close-tab)
     ("[" "Prev tab" tab-bar-switch-to-prev-tab)
     ("]" "Next tab" tab-bar-switch-to-next-tab)]
    ["Undo"
     ("u" "Winner undo" winner-undo)
     ("r" "Winner redo" winner-redo)])

  (global-set-key (kbd "C-c w") #'ian/window-menu))

(provide 'ui-windows)
;;; ui-windows.el ends here
