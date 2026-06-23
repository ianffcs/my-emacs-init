;;; tool-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired file manager configuration and extensions.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. DIRED (Built-in)
;; ============================================================================

(use-package dired
  :straight (:type built-in)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom
  ;; Use ls from coreutils on macOS
  (insert-directory-program (or (executable-find "gls")
                                (executable-find "ls")))
  ;; Listing options — --group-directories-first requires GNU ls (gls)
  (dired-listing-switches (if (executable-find "gls")
                              "-agho --group-directories-first"
                            "-agho"))
  ;; DWIM: use other dired window as default target
  (dired-dwim-target t)
  ;; Recursive operations
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  ;; Kill buffer when opening new dired buffer
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Auto refresh
  (dired-auto-revert-buffer t)
  ;; Use trash
  (delete-by-moving-to-trash t)
  ;; Create destination directories
  (dired-create-destination-dirs 'ask)
  ;; Guess shell commands
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "open" "zathura")
     ("\\.docx?\\'" "open" "libreoffice")
     ("\\.xlsx?\\'" "open" "libreoffice")
     ("\\.pptx?\\'" "open" "libreoffice")
     ("\\.jpe?g\\'" "open" "feh" "gimp")
     ("\\.png\\'" "open" "feh" "gimp")
     ("\\.gif\\'" "open" "feh")
     ("\\.mp4\\'" "open" "mpv" "vlc")
     ("\\.mkv\\'" "open" "mpv" "vlc")
     ("\\.avi\\'" "open" "mpv" "vlc")
     ("\\.mp3\\'" "open" "mpv")
     ("\\.flac\\'" "open" "mpv")
     ("\\.zip\\'" "unzip")
     ("\\.tar\\.gz\\'" "tar xzf")
     ("\\.tar\\.bz2\\'" "tar xjf")))
  :config
  ;; macOS: use gls if available (--group-directories-first already sorts listings)
  (when (eq system-type 'darwin)
    (when-let ((gls (executable-find "gls")))
      (setq insert-directory-program gls)))

  ;; Open file externally
  (defun ian/dired-open-external ()
    "Open file at point with external application."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (pcase system-type
        ('darwin (start-process "dired-open-external" nil "open" file))
        ('gnu/linux (start-process "dired-open-external" nil "xdg-open" file))
        ('windows-nt (w32-shell-execute "open" file))))))

;; Dired-x (extra features)
(use-package dired-x
  :straight (:type built-in)
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$\\|^__pycache__$\\|^\\.DS_Store$")))

;; ============================================================================
;; 2. DIRED EXTENSIONS
;; ============================================================================

;; Subtree - expand directories inline
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)
              ("C-<tab>" . dired-subtree-cycle)
              ("C-x C-n" . dired-subtree-toggle))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'top
        dired-recursive-copies 'always)
  :custom
  (dired-subtree-use-backgrounds nil))

;; ----------------------------------------------------------------------------
;; Dired Tree Sidebar (Treemacs-like)
;; ----------------------------------------------------------------------------

(defgroup ian/dired-tree nil
  "Treemacs-like sidebar using dired-subtree."
  :group 'files)

(defcustom ian/dired-tree-width 36
  "Width of the Dired tree sidebar window."
  :type 'integer
  :group 'ian/dired-tree)

(defcustom ian/dired-tree-buffer-name "*dired-tree*"
  "Buffer name for the Dired tree sidebar."
  :type 'string
  :group 'ian/dired-tree)

(defun ian/project-root-or-default ()
  "Return current project root, or `default-directory` if none."
  (if-let* ((proj (project-current nil)))
      (project-root proj)
    default-directory))

(defun ian/dired-tree--display (dir)
  "Show DIR in the tree sidebar."
  (let* ((buf (get-buffer-create ian/dired-tree-buffer-name))
         (win (display-buffer-in-side-window
               buf `((side . left)
                     (window-width . ,ian/dired-tree-width)
                     (slot . 0)))))
    (with-current-buffer buf
      (setq-local default-directory dir)
      (unless (derived-mode-p 'dired-mode)
        (dired dir))
      ;; Tree vibe
      (setq-local truncate-lines t)
      (hl-line-mode 1)
      (dired-hide-details-mode 1)
      ;; Make it feel "sidebar-ish"
      (setq-local window-size-fixed 'width)
      (setq-local cursor-type nil))
    (set-window-dedicated-p win t)
    win))

(defun ian/dired-tree-open (&optional dir)
  "Open the Dired tree sidebar at DIR (project root by default)."
  (interactive)
  (ian/dired-tree--display (file-name-as-directory (or dir (ian/project-root-or-default)))))

(defun ian/dired-tree-close ()
  "Close the Dired tree sidebar."
  (interactive)
  (when-let ((win (get-buffer-window ian/dired-tree-buffer-name)))
    (delete-window win)))

(defun ian/dired-tree-toggle ()
  "Toggle the Dired tree sidebar."
  (interactive)
  (if (get-buffer-window ian/dired-tree-buffer-name)
      (ian/dired-tree-close)
    (ian/dired-tree-open)))

(defun ian/dired-tree-refresh ()
  "Refresh the sidebar (revert dired)."
  (interactive)
  (when-let ((buf (get-buffer ian/dired-tree-buffer-name)))
    (with-current-buffer buf
      (revert-buffer))))

(defun ian/dired-tree-follow-current ()
  "Jump the tree sidebar to the current buffer's directory."
  (interactive)
  (let ((dir (if-let ((f (buffer-file-name)))
                 (file-name-directory f)
               default-directory)))
    (ian/dired-tree-open dir)))

(defun ian/dired-tree-visit ()
  "In the sidebar: open file/dir in other window, keep sidebar."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        ;; expand/collapse directories with TAB; RET descends into dir
        (dired-subtree-toggle)
      ;; open file in main window
      (select-window (get-mru-window nil nil t))
      (find-file file))))

;; Track the current project root shown in sidebar
(defvar ian/dired-tree--current-root nil
  "The project root currently displayed in the sidebar.")

(defun ian/dired-tree--update-for-project ()
  "Update sidebar to show current project root if sidebar is visible."
  (when-let ((win (get-buffer-window ian/dired-tree-buffer-name)))
    (let ((new-root (ian/project-root-or-default)))
      (unless (equal ian/dired-tree--current-root new-root)
        (setq ian/dired-tree--current-root new-root)
        (ian/dired-tree-open new-root)))))

(defun ian/dired-tree--on-buffer-change ()
  "Called when buffer changes; update sidebar if tracking is enabled."
  (when (and (get-buffer-window ian/dired-tree-buffer-name)
             (not (minibufferp))
             (not (string-prefix-p " " (buffer-name)))
             (not (string= (buffer-name) ian/dired-tree-buffer-name)))
    (ian/dired-tree--update-for-project)))

;; Auto-update on project switch (project.el)
(add-hook 'project-switch-project-hook #'ian/dired-tree--update-for-project)

;; Auto-update on project switch (projectile)
(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook #'ian/dired-tree--update-for-project))

;; Optional: track buffer switches to update sidebar
(defcustom ian/dired-tree-follow-projects t
  "If non-nil, sidebar follows project changes when switching buffers."
  :type 'boolean
  :group 'ian/dired-tree)

(defun ian/dired-tree--maybe-follow ()
  "Update sidebar on buffer switch if `ian/dired-tree-follow-projects' is set."
  (when ian/dired-tree-follow-projects
    (ian/dired-tree--on-buffer-change)))

;; Use a timer to debounce rapid buffer switches
(defvar ian/dired-tree--follow-timer nil)

(defun ian/dired-tree--schedule-follow ()
  "Schedule a sidebar update (debounced)."
  (when (get-buffer-window ian/dired-tree-buffer-name)
    (when ian/dired-tree--follow-timer
      (cancel-timer ian/dired-tree--follow-timer))
    (setq ian/dired-tree--follow-timer
          (run-with-idle-timer 0.3 nil #'ian/dired-tree--maybe-follow))))

(add-hook 'window-buffer-change-functions
          (lambda (_) (ian/dired-tree--schedule-follow)))

;; Dired-mode keybindings
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c t") #'ian/dired-tree-toggle)
  (define-key dired-mode-map (kbd "C-c F") #'ian/dired-tree-follow-current)
  ;; Free C-x C-n from set-goal-column in dired
  (define-key dired-mode-map (kbd "C-x C-n") nil))

;; Sidebar-specific keybindings (applied when buffer is created)
(defun ian/dired-tree--setup-keys ()
  "Setup keybindings for the dired tree sidebar buffer."
  (when (string= (buffer-name) ian/dired-tree-buffer-name)
    (local-set-key (kbd "RET") #'ian/dired-tree-visit)
    (local-set-key (kbd "g") #'ian/dired-tree-refresh)
    (local-set-key (kbd "q") #'ian/dired-tree-close)))

(add-hook 'dired-mode-hook #'ian/dired-tree--setup-keys)

;; Narrow - filter files
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)
              ("C-c n" . dired-narrow-regexp)))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Git info in dired
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Rsync support
(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

;; Collapse empty directories
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

;; Dired filter (advanced filtering)
(use-package dired-filter
  :after dired
  :hook (dired-mode . dired-filter-mode)
  :bind (:map dired-mode-map
              ("C-c /" . dired-filter-mode)))

;; Dired ranger (dual-pane)
(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;; Peep-dired (file preview)
(use-package peep-dired
  :after dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; ============================================================================
;; 3. ICONS IN DIRED
;; ============================================================================

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; ============================================================================
;; 4. DIRED HELPER FUNCTIONS
;; ============================================================================

(defun ian/dired-home ()
  "Open dired in home directory."
  (interactive)
  (dired "~"))

(defun ian/dired-downloads ()
  "Open dired in Downloads directory."
  (interactive)
  (dired "~/Downloads"))

(defun ian/dired-projects ()
  "Open dired in projects directory."
  (interactive)
  (dired "~/src"))

(defun ian/dired-config ()
  "Open dired in Emacs config directory."
  (interactive)
  (dired user-emacs-directory))

(defun ian/dired-org ()
  "Open dired in org directory."
  (interactive)
  (dired (expand-file-name "~/org")))

(defun ian/dired-copy-path ()
  "Copy the path of file at point."
  (interactive)
  (let ((path (dired-get-filename)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun ian/dired-copy-filename ()
  "Copy the filename at point."
  (interactive)
  (let ((filename (dired-get-filename 'no-dir)))
    (kill-new filename)
    (message "Copied: %s" filename)))

;; Add to dired-mode-map
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c p") #'ian/dired-copy-path)
  (define-key dired-mode-map (kbd "C-c w") #'ian/dired-copy-filename))

;; Global keybindings
;;(global-set-key (kbd "C-c d h") #'ian/dired-home)
;;(global-set-key (kbd "C-c d d") #'ian/dired-downloads)
;;(global-set-key (kbd "C-c d p") #'ian/dired-projects)
;;(global-set-key (kbd "C-c d c") #'ian/dired-config)
;;(global-set-key (kbd "C-c d o") #'ian/dired-org)

;; ============================================================================
;; 5. DIRED TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/dired-menu ()
                           "Dired commands"
                           ["Open"
                            ("h" "Home" ian/dired-home)
                            ("d" "Downloads" ian/dired-downloads)
                            ("p" "Projects" ian/dired-projects)
                            ("c" "Config" ian/dired-config)
                            ("o" "Org" ian/dired-org)
                            ("." "Current" dired-jump)]
                           ["Actions"
                            ("n" "Narrow" dired-narrow)
                            ("g" "Git info" dired-git-info-mode)
                            ("e" "Edit (wdired)" wdired-change-to-wdired-mode)
                            ("P" "Peep" peep-dired)])

  (global-set-key (kbd "C-c d") #'ian/dired-menu))

;; dired-sidebar removed: ian/dired-tree (section 2) provides the same sidebar
;; with better project-tracking integration.

(provide 'tool-dired)
;;; tool-dired.el ends here
