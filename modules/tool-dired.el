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
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)
              (")" . dired-git-info-mode)
              ("M-RET" . ian/dired-open-external)
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :custom
  ;; Use ls from coreutils on macOS
  (insert-directory-program (or (executable-find "gls")
                                (executable-find "ls")))
  ;; Listing options
  (dired-listing-switches "-agho --group-directories-first")
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
  ;; macOS: use gls if available
  (when (eq system-type 'darwin)
    (when-let ((gls (executable-find "gls")))
      (setq insert-directory-program gls)))

  ;; Open file externally
  (defun ian/dired-open-external ()
    "Open file at point with external application."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (call-process (pcase system-type
                      ('darwin "open")
                      ('gnu/linux "xdg-open")
                      ('windows-nt "start"))
                    nil 0 nil file))))

;; Dired-x (extra features)
(use-package dired-x
  :straight (:type built-in)
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("." . dired-omit-mode))
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
              ("C-<tab>" . dired-subtree-cycle))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-recursive-deletes 'top
        dired-recursive-copies 'always)
  :custom
  (dired-subtree-use-backgrounds nil))

(defvar ian/dired-tree-buffer-name "*dired-tree*"
  "Name of the Dired tree buffer.")

(defun ian/dired-project-root ()
  "Open Dired at project root in a dedicated buffer."
  (let* ((project (or (when (fboundp 'project-current)
                        (project-current nil))
                      (when (fboundp 'projectile-project-root)
                        (projectile-project-root))))
         (root (cond
                ((and project (fboundp 'project-root))
                 (car (project-roots project)))
                ((stringp project) project)
                (t default-directory))))
    (dired root)
    (rename-buffer ian/dired-tree-buffer-name t)))

(defun ian/dired-tree-visible-p ()
  "Return the window displaying the dired tree, or nil."
  (get-buffer-window ian/dired-tree-buffer-name))

(defun ian/dired-tree-toggle ()
  "Toggle project Dired tree in a left side window."
  (interactive)
  (let ((win (ian/dired-tree-visible-p)))
    (if win
        ;; Close
        (delete-window win)
      ;; Open
      (let ((display-buffer-alist
             `((,ian/dired-tree-buffer-name
                (display-buffer-in-side-window)
                (side . left)
                (slot . 0)
                (window-width . 0.28)
                (window-parameters
                 . ((no-delete-other-windows . t)
                    (no-other-window . t)))))))
        (ian/dired-project-root)
        (display-buffer (get-buffer ian/dired-tree-buffer-name))))))

(global-set-key (kbd "C-x C-n") #'ian/dired-tree-toggle)

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
              ("C-c f" . dired-filter-mode)))

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

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package nerd-icons-dired
  :disabled  ; Using all-the-icons-dired
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

(provide 'tool-dired)
;;; tool-dired.el ends here
