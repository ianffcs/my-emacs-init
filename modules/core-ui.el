;;; core-ui.el --- UI Configuration (Theme, Fonts, Modeline) -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual configuration: themes, fonts, icons, modeline.
;; Migrated from README.org literate config.

;;; Code:

(declare-function parrot-set-parrot-type "parrot")

(require 'seq)

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

;; Frame size: fraction of the current monitor's work area, centered
(defvar ian/frame-screen-fraction 0.85
  "Fraction of the monitor work area a new GUI frame occupies.")

(defun ian/fit-frame-to-monitor (&optional frame)
  "Size and center FRAME on the monitor it is displayed on."
  (let ((frame (or frame (selected-frame))))
    (when (and (display-graphic-p frame)
               (not (frame-parameter frame 'fullscreen)))
      (pcase-let* ((`(,x ,y ,w ,h) (frame-monitor-workarea frame))
                   (fw (round (* w ian/frame-screen-fraction)))
                   (fh (round (* h ian/frame-screen-fraction))))
        (set-frame-size frame fw fh t)
        (set-frame-position frame
                            (+ x (/ (- w fw) 2))
                            (+ y (/ (- h fh) 2)))))))

(add-hook 'window-setup-hook #'ian/fit-frame-to-monitor)
(add-hook 'after-make-frame-functions #'ian/fit-frame-to-monitor)

;; macOS title bar styling — appearance tracks the active theme, not hardcoded dark
(when (eq (ian/platform) 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (let ((hour (string-to-number (format-time-string "%H"))))
    (add-to-list 'default-frame-alist
                 (cons 'ns-appearance
                       (if (and (>= hour 7) (< hour 19)) nil 'dark)))))

;; Fringe
(set-fringe-mode '(8 . 8))

;; Cursor
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)

;; Display buffer rules
(add-to-list 'display-buffer-alist
             '("\\*vterm\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.3)
               (reusable-frames . visible)))
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

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
  :hook (after-init . global-ligature-mode)
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
                            "\\\\" "://")))

;; ============================================================================
;; 3. ICONS
;; ============================================================================

(defun ian/icons-displayable-p (&optional frame)
  "Return non-nil when icon fonts/images should be displayed on FRAME.
FRAME defaults to the selected frame.  TTY Emacs cannot choose fallback
fonts itself; set IAN_EMACS_TTY_ICONS=1 when the surrounding terminal
already uses a Nerd Font."
  (or (display-graphic-p (or frame (selected-frame)))
      (getenv "IAN_EMACS_TTY_ICONS")))

(defvar ian/icon-variables
  '(doom-modeline-icon
    doom-modeline-major-mode-icon
    doom-modeline-buffer-state-icon
    doom-modeline-buffer-modification-icon
    kind-icon-use-icons)
  "Package variables that switch icons on or off.
These are global, not buffer- or frame-local, so `ian/apply-icon-settings'
sets them for the whole session.")

(defun ian/apply-icon-settings (&optional frame)
  "Set `ian/icon-variables' from the display capability of FRAME.
Those packages only offer global switches, so a terminal FRAME never turns
icons off while another live frame can display them."
  (let ((on (and (or (ian/icons-displayable-p frame)
                     (seq-some #'ian/icons-displayable-p (frame-list)))
                 t)))
    (dolist (var ian/icon-variables)
      (set var on))))

(defun ian/apply-icon-settings-after-make-frame (frame)
  "Re-evaluate icon settings when FRAME is created (daemon clients)."
  (ian/apply-icon-settings frame))

(add-hook 'after-make-frame-functions #'ian/apply-icon-settings-after-make-frame)
(add-hook 'after-init-hook #'ian/apply-icon-settings)
(ian/apply-icon-settings)

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . (lambda ()
                              (when (ian/icons-displayable-p)
                                (nerd-icons-completion-marginalia-setup))))
  :config
  (when (ian/icons-displayable-p)
    (nerd-icons-completion-mode)))

;; ============================================================================
;; 3.5 EMOJI
;; ============================================================================

(use-package emojify
  :hook ((org-mode . emojify-mode)
         (markdown-mode . emojify-mode))
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode)))

;; ============================================================================
;; 4. THEMES
;; ============================================================================

;; Modus themes — dark theme (modus-vivendi) still used at night
(use-package modus-themes
  :demand t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-headings '((1 . (rainbow overline background 1.4))
                           (2 . (rainbow background 1.3))
                           (3 . (rainbow bold 1.2))
                           (t . (semilight 1.1)))))

;; Catppuccin — light theme (Latte flavour)
(use-package catppuccin-theme
  :straight t
  :demand t
  :custom
  (catppuccin-flavor 'latte))

;; Theme switcher
(defvar ian/light-theme 'catppuccin)
(defvar ian/dark-theme 'modus-vivendi)

(defun ian/theme-sync-appearance (theme)
  "Sync macOS title bar appearance to THEME on darwin."
  (when (eq (ian/platform) 'darwin)
    (set-frame-parameter nil 'ns-appearance
                         (if (eq theme ian/light-theme) nil 'dark))))

(defun ian/activate-theme (theme)
  "Enable only THEME and synchronize frame appearance."
  (unless (equal custom-enabled-themes (list theme))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))
  (ian/theme-sync-appearance theme))

(defun ian/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (ian/activate-theme
   (if (eq (car custom-enabled-themes) ian/dark-theme)
       ian/light-theme
     ian/dark-theme)))

;; Auto-switch based on time.  Prefer a graphical frame when a daemon has one.
(defun ian/auto-theme (&optional frame)
  "Switch theme based on time for FRAME.
Use an existing graphical frame when FRAME is nil; terminal-only Emacs is dark."
  (let* ((frame (or frame
                    (seq-find #'display-graphic-p (frame-list))
                    (selected-frame)))
         (hour (string-to-number (format-time-string "%H")))
         (want (if (display-graphic-p frame)
                   (if (and (>= hour 7) (< hour 19)) ian/light-theme ian/dark-theme)
                 ian/dark-theme)))
    (ian/activate-theme want)))

(defun ian/auto-theme-after-make-frame (frame)
  "Synchronize the theme when a graphical FRAME is created."
  (when (display-graphic-p frame)
    (ian/auto-theme frame)))

(add-hook 'after-init-hook #'ian/auto-theme)
(add-hook 'after-make-frame-functions #'ian/auto-theme-after-make-frame)
(run-at-time "1 hour" 3600 #'ian/auto-theme)
;; ============================================================================
;; 5. MODELINE
;; ============================================================================

;; Nyan cat progress indicator
(use-package nyan-mode
  :hook (after-init . nyan-mode)
  :custom
  (nyan-animate-nyancat nil)
  (nyan-wavy-trail t))

;; Parrot animation (manual trigger only — was firing on every save)
(use-package parrot
  :hook (after-init . parrot-mode)
  :config
  (parrot-set-parrot-type 'emacs))

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
  ;; Icon on/off switches are set per frame by `ian/apply-icon-settings'.
  (doom-modeline-major-mode-color-icon t)
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
;; 9. BEACON (Cursor beacon)
;; ============================================================================

(use-package beacon
  :diminish
  :hook (after-init . beacon-mode)
  :custom
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-point-moves nil)
  (beacon-blink-duration 0.3)
  (beacon-size 40)
  (beacon-color "#5B6268"))

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
  :hook (after-init . global-page-break-lines-mode))

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
