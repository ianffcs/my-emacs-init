;;; core-ui.el --- UI Configuration (Theme, Fonts, Modeline) -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual configuration: themes, fonts, icons, modeline.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. FRAME SETTINGS
;; ============================================================================

;; Disable GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Frame settings
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b - Emacs"))

;; Default frame size
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))

;; Fringe
(set-fringe-mode '(8 . 8))

;; Cursor
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)

;; ============================================================================
;; 2. FONTS
;; ============================================================================

(defvar ian/default-font-size 140)
(defvar ian/default-variable-font-size 140)

(defvar ian/font-family-monospace
  (cond
   ((find-font (font-spec :name "JetBrains Mono")) "JetBrains Mono")
   ((find-font (font-spec :name "Fira Code")) "Fira Code")
   ((find-font (font-spec :name "Source Code Pro")) "Source Code Pro")
   ((find-font (font-spec :name "Menlo")) "Menlo")
   ((find-font (font-spec :name "Monaco")) "Monaco")
   ((find-font (font-spec :name "Consolas")) "Consolas")
   (t "Monospace")))

(defvar ian/font-family-variable
  (cond
   ((find-font (font-spec :name "Cantarell")) "Cantarell")
   ((find-font (font-spec :name "SF Pro")) "SF Pro")
   ((find-font (font-spec :name "Helvetica Neue")) "Helvetica Neue")
   ((find-font (font-spec :name "Arial")) "Arial")
   (t "Sans Serif")))

(defun ian/set-fonts ()
  "Set default fonts."
  (set-face-attribute 'default nil
                      :font ian/font-family-monospace
                      :height ian/default-font-size
                      :weight 'regular)
  (set-face-attribute 'fixed-pitch nil
                      :font ian/font-family-monospace
                      :height ian/default-font-size)
  (set-face-attribute 'variable-pitch nil
                      :font ian/font-family-variable
                      :height ian/default-variable-font-size
                      :weight 'regular))

;; Apply fonts
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (ian/set-fonts))))
  (ian/set-fonts))

;; Font scaling
(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset))
  :config
  (default-text-scale-mode))

;; Ligatures (for supported fonts)
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  (global-ligature-mode t))

;; ============================================================================
;; 3. ICONS
;; ============================================================================

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

;; ============================================================================
;; 3.5 EMOJI
;; ============================================================================

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode)))

;; ============================================================================
;; 4. THEMES
;; ============================================================================

;; Modus themes (built-in in Emacs 28+)
(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-headings '((1 . (rainbow overline background 1.4))
                           (2 . (rainbow background 1.3))
                           (3 . (rainbow bold 1.2))
                           (t . (semilight 1.1))))
  :config
  (modus-themes-load-theme 'modus-vivendi))

;; Ef themes
(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-day ef-night)))

;; Catppuccin
(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'mocha))

;; Theme switcher
(defvar ian/light-theme 'modus-operandi)
(defvar ian/dark-theme 'modus-vivendi)

(defun ian/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) ian/dark-theme)
      (progn
        (disable-theme ian/dark-theme)
        (load-theme ian/light-theme t))
    (disable-theme ian/light-theme)
    (load-theme ian/dark-theme t)))

(global-set-key (kbd "C-c t t") #'ian/toggle-theme)

;; Auto-switch based on time
(defun ian/auto-theme ()
  "Automatically switch theme based on time of day."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (and (>= hour 7) (< hour 19))
        (load-theme ian/light-theme t)
      (load-theme ian/dark-theme t))))

;; (add-hook 'after-init-hook #'ian/auto-theme)
;; (run-at-time "1 hour" 3600 #'ian/auto-theme)
;; ============================================================================
;; 5. MODELINE
;; ============================================================================

;; Mode icons in modeline
(use-package mode-icons
  :config
  (mode-icons-mode))

;; Nyan cat progress indicator
(use-package nyan-mode
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t)
  :config
  (nyan-mode 1))

;; Parrot animation on save
(use-package parrot
  :config
  (parrot-set-parrot-type 'emacs)
  (parrot-mode)
  (add-hook 'before-save-hook #'parrot-start-animation))

;; Doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud nil)
  (doom-modeline-window-width-limit 85)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-env-version t)
  (doom-modeline-time nil)
  (doom-modeline-modal nil))

;; Column number
(column-number-mode 1)

;; Size indication
(size-indication-mode 1)

;; Time in modeline (optional)
;; (display-time-mode 1)
;; (setq display-time-format "%H:%M"
;;       display-time-default-load-average nil)

;; Battery (for laptops)
;; (display-battery-mode 1)

;; ============================================================================
;; 6. MINIONS (Hide minor modes)
;; ============================================================================

(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minions-mode-line-lighter "…"))

;; ============================================================================
;; 7. SOLAIRE (Visual distinction for real buffers)
;; ============================================================================

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;; ============================================================================
;; 8. HL-LINE (Highlight current line)
;; ============================================================================

(global-hl-line-mode 1)

;; ============================================================================
;; 9. PULSE (Visual feedback)
;; ============================================================================

(use-package pulse
  :straight (:type built-in)
  :custom
  (pulse-delay 0.04)
  (pulse-iterations 10)
  :config
  (defun ian/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point))))

;; ============================================================================
;; 10. BEACON (Cursor beacon)
;; ============================================================================

(use-package beacon
  :diminish
  :custom
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-point-moves nil)
  (beacon-blink-duration 0.3)
  (beacon-size 40)
  (beacon-color "#5B6268")
  :config
  (beacon-mode 1))

;; ============================================================================
;; 11. VISUAL BELL
;; ============================================================================

(setq ring-bell-function 'ignore
      visible-bell nil)

;; ============================================================================
;; 12. DIALOG BOXES
;; ============================================================================

(setq use-dialog-box nil
      use-file-dialog nil)

;; ============================================================================
;; 13. YES/NO -> Y/N
;; ============================================================================

(setq use-short-answers t)

;; ============================================================================
;; 14. TRANSPARENCY (Optional)
;; ============================================================================

(defun ian/set-frame-alpha (value)
  "Set frame transparency to VALUE (0-100)."
  (interactive "nAlpha value (0-100): ")
  (set-frame-parameter nil 'alpha-background value))

;; (ian/set-frame-alpha 95)

;; ============================================================================
;; 15. PAGE BREAK LINES
;; ============================================================================

(use-package page-break-lines
  :diminish
  :config
  (global-page-break-lines-mode))

;; ============================================================================
;; 16. TRANSIENT UI MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/ui-menu ()
                           "UI customization commands"
                           ["Theme"
                            ("t" "Toggle dark/light" ian/toggle-theme)
                            ("T" "Choose theme" consult-theme)]
                           ["Font"
                            ("+" "Increase size" default-text-scale-increase)
                            ("-" "Decrease size" default-text-scale-decrease)
                            ("0" "Reset size" default-text-scale-reset)]
                           ["Display"
                            ("l" "Line numbers" display-line-numbers-mode)
                            ("h" "Highlight line" hl-line-mode)
                            ("w" "Whitespace" whitespace-mode)
                            ("v" "Visual line" visual-line-mode)]
                           ["Frame"
                            ("f" "Fullscreen" toggle-frame-fullscreen)
                            ("m" "Maximize" toggle-frame-maximized)]
                           ["Inspect"
                            ("?" "What face" ian/what-face)])

  (global-set-key (kbd "C-c u") #'ian/ui-menu))

;; ============================================================================
;; 17. HELPER FUNCTIONS
;; ============================================================================

(defun ian/what-face (pos)
  "Show the face at POS."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(provide 'core-ui)
;;; core-ui.el ends here
