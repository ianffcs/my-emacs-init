;;; tool-shell.el --- Shell and terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Eshell, vterm, and terminal emulator configuration.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. ESHELL
;; ============================================================================

(use-package eshell
  :straight (:type built-in)
  :bind ("C-c t e" . eshell)
  :hook ((eshell-mode . (lambda ()
                          (setq-local scroll-margin 0)
                          (setq-local truncate-lines t)
                          (setq-local global-hl-line-mode nil))))
  :custom
  (eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-banner-message "")
  (eshell-visual-commands '("htop" "top" "less" "more" "vi" "vim" "nvim"
                            "screen" "tmux" "ncdu" "btop" "watch"
                            "tail" "ssh" "pulsemixer"))
  (eshell-visual-subcommands '(("git" "log" "diff" "show")))
  :config
  ;; Aliases
  (defalias 'eshell/ll (lambda () (eshell/ls "-la")))
  (defalias 'eshell/la (lambda () (eshell/ls "-la")))
  (defalias 'eshell/.. (lambda () (eshell/cd "..")))
  (defalias 'eshell/... (lambda () (eshell/cd "../..")))

  ;; Custom commands
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun eshell/d ()
    "Open dired in current directory."
    (dired "."))

  (defun eshell/e (file)
    "Open FILE in Emacs."
    (find-file file))

  (defun eshell/mkcd (dir)
    "Make directory DIR and cd into it."
    (make-directory dir t)
    (eshell/cd dir))

  (defun eshell/gst ()
    "Git status."
    (magit-status))

  (defun eshell/ff (pattern)
    "Find files matching PATTERN."
    (eshell-command-result (format "find . -name '*%s*'" pattern))))

;; Eshell prompt
(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

;; Eshell syntax highlighting
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; Eshell auto-suggestions
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :custom
  (esh-autosuggest-delay 0.5))

;; Fish completion in eshell
(use-package fish-completion
  :when (executable-find "fish")
  :hook (eshell-mode . fish-completion-mode))

;; Eshell z (directory jumping)
(use-package eshell-z
  :after eshell
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

;; ============================================================================
;; 2. VTERM (Fast Terminal)
;; ============================================================================

(use-package vterm
  :commands vterm
  :bind (("C-c t v" . vterm)
         ("C-c t V" . vterm-other-window)
         :map vterm-mode-map
         ("C-c C-c" . vterm-send-C-c)
         ("C-c C-d" . vterm-send-C-d)
         ("C-q" . vterm-send-next-key))
  :custom
  (vterm-shell (or (executable-find "zsh")
                   (executable-find "bash")
                   "/bin/sh"))
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  (vterm-always-compile-module t)
  :config
  ;; Display vterm in bottom window
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.3)
                 (reusable-frames . visible)))

  ;; Directory tracking
  (defun ian/vterm-directory-sync ()
    "Sync vterm directory with default-directory."
    (when vterm--process
      (let* ((pid (process-id vterm--process))
             (dir (file-truename (format "/proc/%d/cwd" pid))))
        (when (file-directory-p dir)
          (setq default-directory dir)))))

  ;; Project-specific vterm
  (defun ian/vterm-project ()
    "Open vterm in project root."
    (interactive)
    (let ((default-directory (or (projectile-project-root) default-directory)))
      (vterm (format "*vterm[%s]*" (projectile-project-name)))))

  ;; Named vterm
  (defun ian/vterm-named (name)
    "Open vterm with NAME."
    (interactive "sVterm name: ")
    (vterm (format "*vterm[%s]*" name))))

;; Vterm toggle
(use-package vterm-toggle
  :after vterm
  :bind (("C-c t t" . vterm-toggle)
         ("C-c t T" . vterm-toggle-cd))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-fullscreen-p nil)
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil))

;; Multi-vterm (multiple terminals)
(use-package multi-vterm
  :after vterm
  :bind (("C-c t n" . multi-vterm)
         ("C-c t p" . multi-vterm-project)
         ("C-c t ]" . multi-vterm-next)
         ("C-c t [" . multi-vterm-prev)))

;; ============================================================================
;; 3. EAT (Emulate A Terminal)
;; ============================================================================

(use-package eat
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi" "*.ti"
                           ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :hook ((eshell-first-time-mode . eat-eshell-visual-command-mode)
         (eshell-first-time-mode . eat-eshell-mode))
  :custom
  (eat-shell (or (executable-find "zsh")
                 (executable-find "bash")
                 "/bin/sh"))
  (eat-kill-buffer-on-exit t)
  :config
  (eat-eshell-visual-command-mode +1))

;; ============================================================================
;; 4. COMINT (Common Shell Interface)
;; ============================================================================

(use-package comint
  :straight (:type built-in)
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-output 'others)
  (comint-move-point-for-output 'others)
  (comint-buffer-maximum-size 10000)
  :config
  ;; Colorize output
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; ============================================================================
;; 5. SHELL-POP (Quick Shell Access)
;; ============================================================================

(use-package shell-pop
  :bind ("C-c t s" . shell-pop)
  :custom
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm))))
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))

;; ============================================================================
;; 6. TERMINAL HELPER FUNCTIONS
;; ============================================================================

(defun ian/eshell-here ()
  "Open eshell in current directory."
  (interactive)
  (let ((default-directory default-directory))
    (eshell 'new)))

(defun ian/eshell-project ()
  "Open eshell in project root."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (eshell 'new)))

(defun ian/term-send-raw-meta ()
  "Send ESC in term-mode."
  (interactive)
  (term-send-raw-string "\e"))

;; Additional keybindings
(global-set-key (kbd "C-c t h") #'ian/eshell-here)
(global-set-key (kbd "C-c t P") #'ian/eshell-project)

;; ============================================================================
;; 7. TERMINAL TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/terminal-menu ()
    "Terminal commands"
    ["Vterm"
     ("v" "Vterm" vterm)
     ("V" "Vterm other window" vterm-other-window)
     ("t" "Toggle" vterm-toggle)
     ("n" "New vterm" multi-vterm)
     ("p" "Project vterm" multi-vterm-project)]
    ["Eshell"
     ("e" "Eshell" eshell)
     ("h" "Eshell here" ian/eshell-here)
     ("P" "Eshell project" ian/eshell-project)]
    ["Navigate"
     ("[" "Prev vterm" multi-vterm-prev)
     ("]" "Next vterm" multi-vterm-next)
     ("s" "Shell pop" shell-pop)])

  (global-set-key (kbd "C-c t") #'ian/terminal-menu))

(provide 'tool-shell)
;;; tool-shell.el ends here

