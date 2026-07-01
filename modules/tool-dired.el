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
  :custom
  (dired-subtree-use-backgrounds nil))

;; Dired-mode keybindings
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "TAB") #'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "<backtab>") #'dired-subtree-cycle)
  (define-key dired-mode-map (kbd "C-<tab>") #'dired-subtree-cycle))

;; ----------------------------------------------------------------------------
;; Project sidebar — dired in a left side window, dired-subtree for expansion
;; ----------------------------------------------------------------------------

(defvar ian/sidebar-buffer-name " *dired-sidebar*")
(defvar ian/sidebar-width 36)

(defun ian/dired-tree--current-path ()
  "Return the file or directory that the Dired tree should reveal."
  (cond
   ((derived-mode-p 'dired-mode)
    (or (ignore-errors (dired-get-filename nil t))
        default-directory))
   (buffer-file-name)
   (default-directory)))

(defun ian/dired-tree--path-directory (path)
  "Return the containing directory for PATH."
  (file-name-as-directory
   (if (and path (file-directory-p path))
       path
     (or (and path (file-name-directory path))
         default-directory))))

(defun ian/sidebar--project-dir (&optional path)
  "Return the project root for PATH, or `default-directory'."
  (let ((default-directory (ian/dired-tree--path-directory
                            (or path default-directory))))
    (if-let* ((proj (project-current nil)))
      (project-root proj)
      default-directory)))

(defun ian/dired-tree--display (dir)
  "Display a Dired sidebar rooted at DIR and return its window."
  (let* ((buf (dired-noselect (file-name-as-directory dir)))
         (old (get-buffer ian/sidebar-buffer-name)))
    (when (and old (not (eq old buf)))
      (kill-buffer old))
    (with-current-buffer buf
      (unless (string= (buffer-name) ian/sidebar-buffer-name)
        (rename-buffer ian/sidebar-buffer-name))
      (setq-local truncate-lines t
                  window-size-fixed 'width
                  cursor-type nil))
    (let ((win (display-buffer-in-side-window
                buf `((side . left)
                      (window-width . ,ian/sidebar-width)
                      (slot . 0)))))
      (set-window-dedicated-p win t)
      win)))

(defun ian/dired-tree--expand-to (root target)
  "Expand Dired subtree entries from ROOT down to TARGET."
  (require 'dired-subtree nil t)
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (target (expand-file-name target))
         (target-dir (ian/dired-tree--path-directory target)))
    (when (file-in-directory-p target root)
      (let ((dir root))
        (dolist (part (split-string (file-relative-name target-dir root) "/" t))
          (setq dir (expand-file-name part (file-name-as-directory dir)))
          (when (dired-goto-file dir)
            (dired-subtree-insert))))
      (dired-goto-file target))))

(defun ian/dired-tree-follow-current ()
  "Show the current file or directory in the project Dired tree."
  (interactive)
  (let* ((target (ian/dired-tree--current-path))
         (dir (ian/sidebar--project-dir target))
         (win (ian/dired-tree--display dir)))
    (with-selected-window win
      (ian/dired-tree--expand-to dir target))))

(defun ian/sidebar-toggle ()
  "Toggle a dired sidebar on the left for the current project root."
  (interactive)
  (if-let ((win (get-buffer-window ian/sidebar-buffer-name)))
      (delete-window win)
    (ian/dired-tree-follow-current)))

(defalias 'ian/dired-tree-toggle #'ian/sidebar-toggle)

(global-set-key (kbd "C-x C-n") #'ian/sidebar-toggle)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c t") #'ian/dired-tree-toggle)
  (define-key dired-mode-map (kbd "C-c F") #'ian/dired-tree-follow-current))

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
  :hook (dired-mode . (lambda ()
                        (when (ian/icons-displayable-p)
                          (nerd-icons-dired-mode)))))

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
