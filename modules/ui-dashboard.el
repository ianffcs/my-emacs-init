;;; ui-dashboard.el --- The Startup Screen -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard configuration for startup screen.
;; Includes project bookmarks, recent files, agenda, and quick access.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. PRE-REQUISITES
;; ============================================================================

(use-package page-break-lines
  :diminish
  :config
  (global-page-break-lines-mode))

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
;; 2. DASHBOARD CONFIGURATION
;; ============================================================================

(use-package dashboard
  :init
  ;; --- Critical settings (must be in :init) ---
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-display-icons-p t)

  ;; Dashboard content layout
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 10)
                          (agenda . 5)
                          (registers . 3)))

  ;; Set custom headings
  (setq dashboard-item-names
        '((recents . "Recent Files")
          (bookmarks . "Bookmarks")
          (projects . "Projects")
          (agenda . "Upcoming")
          (registers . "Registers")))

  ;; Short item generators
  (setq dashboard-item-shortcuts
        '((recents . "r")
          (bookmarks . "m")
          (projects . "p")
          (agenda . "a")
          (registers . "e")))

  ;; Project integration
  (with-eval-after-load 'projectile
    (setq dashboard-projects-switch-function 'projectile-switch-project-by-name))

  :config
  ;; --- Visual Configuration ---
  (setq dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts t
        dashboard-page-separator "\n\n")

  ;; --- Banner Configuration ---
  ;; Options: 'official, 'logo, 1-3 for text banners, or path to image
  (setq dashboard-startup-banner 'logo)
  ;; Custom ASCII banner (uncomment to use)
  ;; (setq dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))

  ;; Banner title
  (setq dashboard-banner-logo-title
        (format "Welcome back, %s! Emacs loaded in %.2f seconds with %d packages."
                user-login-name
                (float-time (time-subtract after-init-time before-init-time))
                (length package-activated-list)))

  ;; --- Footer Configuration ---
  (setq dashboard-set-footer t
        dashboard-footer-icon (nerd-icons-octicon "nf-oct-heart" :height 1.1 :v-adjust -0.05 :face 'error)
        dashboard-footer-messages
        '("Happy Hacking, Ian!"
          "One editor to rule them all."
          "M-x butterfly"
          "Complexity is the enemy."
          "There is no system but GNU, and Linux is one of its kernels."
          "Emacs: Eight Megabytes And Constantly Swapping"
          "The only true IDE."
          "While any text editor can save your files, only Emacs can save your soul."))

  ;; --- Projects Backend ---
  (with-eval-after-load 'projectile
    (setq dashboard-projects-backend 'projectile))

  ;; --- Agenda Configuration ---
  (setq dashboard-week-agenda t
        dashboard-agenda-release-buffers t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-agenda-sort-strategy '(time-up priority-down category-up)
        dashboard-agenda-prefix-format " %i %-12:c%?-12t% s"
        dashboard-agenda-time-string-format "%Y-%m-%d %H:%M")

  ;; --- Navigator Buttons ---
  (setq dashboard-set-navigator t
        dashboard-navigator-buttons
        `(;; Row 1
          ((,(nerd-icons-octicon "nf-oct-gear" :height 1.1 :v-adjust 0.0)
            "Config"
            "Open Emacs configuration"
            (lambda (&rest _) (find-file (expand-file-name "init.el" user-emacs-directory))))

           (,(nerd-icons-octicon "nf-oct-package" :height 1.1 :v-adjust 0.0)
            "Update"
            "Update packages"
            (lambda (&rest _) (straight-pull-all)))

           (,(nerd-icons-faicon "nf-fa-calendar" :height 1.1 :v-adjust 0.0)
            "Agenda"
            "Open org agenda"
            (lambda (&rest _) (org-agenda nil "d")))

           (,(nerd-icons-octicon "nf-oct-file_code" :height 1.1 :v-adjust 0.0)
            "Scratch"
            "Open scratch buffer"
            (lambda (&rest _) (switch-to-buffer "*scratch*"))))

          ;; Row 2
          ((,(nerd-icons-faicon "nf-fa-folder_open" :height 1.1 :v-adjust 0.0)
            "Projects"
            "Browse projects"
            (lambda (&rest _) (projectile-switch-project)))

           (,(nerd-icons-octicon "nf-oct-book" :height 1.1 :v-adjust 0.0)
            "Notes"
            "Open org-roam"
            (lambda (&rest _) (org-roam-node-find)))

           (,(nerd-icons-faicon "nf-fa-search" :height 1.1 :v-adjust 0.0)
            "Grep"
            "Search with ripgrep"
            (lambda (&rest _) (consult-ripgrep)))

           (,(nerd-icons-octicon "nf-oct-mark_github" :height 1.1 :v-adjust 0.0)
            "GitHub"
            "Open GitHub"
            (lambda (&rest _) (browse-url "https://github.com"))))))

  ;; --- Keybindings ---
  (setq dashboard-set-init-info t)

  ;; Enable dashboard
  (dashboard-setup-startup-hook)

  ;; Refresh if buffer exists
  (when (get-buffer "*dashboard*")
    (dashboard-refresh-buffer)))

;; Dashboard keybindings
(with-eval-after-load 'dashboard
  (define-key dashboard-mode-map (kbd "g") #'dashboard-refresh-buffer)
  (define-key dashboard-mode-map (kbd "q") #'quit-window)
  (define-key dashboard-mode-map (kbd "n") #'dashboard-next-line)
  (define-key dashboard-mode-map (kbd "p") #'dashboard-previous-line)
  (define-key dashboard-mode-map (kbd "f") #'find-file)
  (define-key dashboard-mode-map (kbd "b") #'bookmark-jump)
  (define-key dashboard-mode-map (kbd "s") #'eshell)
  (define-key dashboard-mode-map (kbd "v") #'vterm)
  (define-key dashboard-mode-map (kbd "?") #'dashboard-help))

;; Help function
(defun dashboard-help ()
  "Show dashboard help."
  (interactive)
  (message "g:refresh n/p:navigate r:recents m:bookmarks p:projects a:agenda f:file b:bookmark s:shell v:vterm q:quit"))

;; ============================================================================
;; 3. PROJECT BOOKMARKS
;; ============================================================================

(defun ian/register-src-projects ()
  "Register all directories in ~/src/ as projectile projects."
  (let ((src-dir (expand-file-name "~/src/")))
    (when (file-directory-p src-dir)
      (dolist (dir (directory-files src-dir t "^[^.]"))
        (when (and (file-directory-p dir)
                   (not (string-match-p "\\.DS_Store\\|\\.env\\|\\.zip$\\|node_modules" dir)))
          (projectile-add-known-project dir))))))

(defun ian/register-work-projects ()
  "Register all directories in ~/work/ as projectile projects."
  (let ((work-dir (expand-file-name "~/work/")))
    (when (file-directory-p work-dir)
      (dolist (dir (directory-files work-dir t "^[^.]"))
        (when (and (file-directory-p dir)
                   (not (string-match-p "\\.DS_Store\\|\\.env\\|\\.zip$\\|node_modules" dir)))
          (projectile-add-known-project dir))))))

(defun ian/register-github-projects ()
  "Register all directories in ~/github/ as projectile projects."
  (let ((github-dir (expand-file-name "~/github/")))
    (when (file-directory-p github-dir)
      (dolist (dir (directory-files github-dir t "^[^.]"))
        (when (and (file-directory-p dir)
                   (not (string-match-p "\\.DS_Store\\|\\.env\\|\\.zip$\\|node_modules" dir)))
          (projectile-add-known-project dir))))))

;; Register all project directories on startup
(with-eval-after-load 'projectile
  (ian/register-src-projects)
  (ian/register-work-projects)
  (ian/register-github-projects))

;; ============================================================================
;; 4. DESKTOP INTEGRATION
;; ============================================================================

;; Prevent desktop-save-mode from saving dashboard buffer
(with-eval-after-load 'desktop
  (add-to-list 'desktop-modes-not-to-save 'dashboard-mode))

;; ============================================================================
;; 5. STARTUP FUNCTIONS
;; ============================================================================

(defun ian/force-dashboard-startup ()
  "Ensure dashboard is shown on startup."
  (when (< (length command-line-args) 2)  ; Only if no file arguments
    (dashboard-open)))

(add-hook 'window-setup-hook #'ian/force-dashboard-startup)

(defun ian/dashboard-goto-recent-files ()
  "Jump to recent files section."
  (interactive)
  (dashboard-open)
  (dashboard-jump-to-recents))

(defun ian/dashboard-goto-projects ()
  "Jump to projects section."
  (interactive)
  (dashboard-open)
  (dashboard-jump-to-projects))

(defun ian/dashboard-goto-bookmarks ()
  "Jump to bookmarks section."
  (interactive)
  (dashboard-open)
  (dashboard-jump-to-bookmarks))

(defun ian/dashboard-goto-agenda ()
  "Jump to agenda section."
  (interactive)
  (dashboard-open)
  (dashboard-jump-to-agenda))

;; Global dashboard access
;; (global-set-key (kbd "C-c h h") #'dashboard-open)
;;(global-set-key (kbd "C-c h r") #'ian/dashboard-goto-recent-files)
;;(global-set-key (kbd "C-c h p") #'ian/dashboard-goto-projects)
;;(global-set-key (kbd "C-c h b") #'ian/dashboard-goto-bookmarks)
;;(global-set-key (kbd "C-c h a") #'ian/dashboard-goto-agenda)

;; ============================================================================
;; 6. CUSTOM WIDGETS
;; ============================================================================

;; Weather widget (requires request.el)
(defvar ian/dashboard-weather-cache nil
  "Cache for weather data.")

(defvar ian/dashboard-weather-location "Vienna,AT"
  "Location for weather widget.")

(defun ian/dashboard-weather-widget (&rest _)
  "Insert weather widget into dashboard."
  (when ian/dashboard-weather-cache
    (insert "\n")
    (widget-insert (propertize ian/dashboard-weather-cache
                               'face 'dashboard-footer))))

;; Quick access widget
(defun ian/dashboard-quick-access-widget (&rest _)
  "Insert quick access links."
  (insert "\n")
  (widget-insert (propertize "Quick Access: " 'face 'dashboard-heading))
  (insert "[")
  (widget-create 'push-button
                 :action (lambda (&rest _) (find-file "~/org/inbox.org"))
                 :button-face 'dashboard-navigator
                 "Inbox")
  (insert "] [")
  (widget-create 'push-button
                 :action (lambda (&rest _) (find-file "~/org/todo.org"))
                 :button-face 'dashboard-navigator
                 "Todo")
  (insert "] [")
  (widget-create 'push-button
                 :action (lambda (&rest _) (find-file "~/org/notes.org"))
                 :button-face 'dashboard-navigator
                 "Notes")
  (insert "] [")
  (widget-create 'push-button
                 :action (lambda (&rest _) (dired "~/Downloads"))
                 :button-face 'dashboard-navigator
                 "Downloads")
  (insert "]\n"))

;; System info widget
(defun ian/dashboard-system-info-widget (&rest _)
  "Insert system information widget."
  (insert "\n")
  (let* ((uptime (emacs-uptime))
         (gc-count gcs-done)
         (gc-time (format "%.2f" gc-elapsed)))
    (widget-insert
     (propertize
      (format "Emacs Uptime: %s | GC: %d (%.2fs) | Buffers: %d"
              uptime gc-count gc-elapsed (length (buffer-list)))
      'face 'dashboard-footer))))

;; Add custom widgets (uncomment to enable)
;; (add-to-list 'dashboard-item-generators '(quick-access . ian/dashboard-quick-access-widget))
;; (add-to-list 'dashboard-item-generators '(system-info . ian/dashboard-system-info-widget))
;; (add-to-list 'dashboard-items '(quick-access) t)
;; (add-to-list 'dashboard-items '(system-info) t)

;; ============================================================================
;; 7. CUSTOM BANNER
;; ============================================================================

(defvar ian/dashboard-banners
  '("
    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
    █████╗  ██╔████╔██║███████║██║     ███████╗
    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
"
    "
     _____
    | ____|_ __ ___   __ _  ___ ___
    |  _| | '_ ` _ \\ / _` |/ __/ __|
    | |___| | | | | | (_| | (__\\__ \\
    |_____|_| |_| |_|\\__,_|\\___|___/
"
    "
    ┌─┐┌┬┐┌─┐┌─┐┌─┐
    ├┤ │││├─┤│  └─┐
    └─┘┴ ┴┴ ┴└─┘└─┘
")
  "List of ASCII banners for dashboard.")

(defun ian/dashboard-random-banner ()
  "Return a random banner from the list."
  (nth (random (length ian/dashboard-banners)) ian/dashboard-banners))

;; Use random ASCII banner (uncomment to enable)
;; (setq dashboard-startup-banner (ian/dashboard-random-banner))

;; ============================================================================
;; 8. RECENT FILES FILTER
;; ============================================================================

;; Filter out certain files from recent files list
(setq dashboard-recentf-show-base-path t)

(defun ian/dashboard-filter-recentf (entry)
  "Filter ENTRY from recent files."
  (let ((ignored-patterns '("\\.elc$"
                            "\\.pyc$"
                            "COMMIT_EDITMSG"
                            "MERGE_MSG"
                            "\\.git/"
                            "/tmp/"
                            "/var/"
                            "recentf"
                            "bookmarks"
                            "\\.cache/"
                            "node_modules/"
                            "straight/build/"
                            "elpa/")))
    (not (cl-some (lambda (pattern)
                    (string-match-p pattern entry))
                  ignored-patterns))))

(with-eval-after-load 'dashboard
  (setq dashboard-recentf-item-format "%s  %s"
        dashboard-filter-recentf-function #'ian/dashboard-filter-recentf))

;; ============================================================================
;; 9. DASHBOARD TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/dashboard-menu ()
    "Dashboard quick access menu"
    ["Navigation"
     ("h" "Dashboard" dashboard-open)
     ("r" "Recent files" ian/dashboard-goto-recent-files)
     ("p" "Projects" ian/dashboard-goto-projects)
     ("b" "Bookmarks" ian/dashboard-goto-bookmarks)
     ("a" "Agenda" ian/dashboard-goto-agenda)]
    ["Actions"
     ("g" "Refresh" dashboard-refresh-buffer)
     ("f" "Find file" find-file)
     ("P" "Switch project" projectile-switch-project)
     ("n" "Org-roam" org-roam-node-find)]
    ["Quick Open"
     ("i" "Inbox" (lambda () (interactive) (find-file "~/org/inbox.org")))
     ("t" "Todo" (lambda () (interactive) (find-file "~/org/todo.org")))
     ("c" "Config" (lambda () (interactive) (find-file user-init-file)))
     ("s" "Scratch" (lambda () (interactive) (switch-to-buffer "*scratch*")))])

  (global-set-key (kbd "C-c h") #'ian/dashboard-menu))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here
