;;; core-session.el --- Session Management & Alerts -*- lexical-binding: t; -*-

;;; Commentary:
;; Desktop session persistence and notification system.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. DESKTOP MODE (Session Restore)
;; ============================================================================

(use-package desktop
  :straight (:type built-in)
  :if window-system
  :hook ((after-init . ian/desktop-restore)
         (desktop-after-read . ian/desktop-remove))
  :custom
  (desktop-path `(,user-emacs-directory))
  (desktop-dirname user-emacs-directory)
  (desktop-base-file-name "desktop")
  (desktop-base-lock-name "desktop.lock")
  (desktop-save t)
  (desktop-load-locked-desktop t)
  (desktop-restore-eager 5)
  (desktop-auto-save-timeout 300)
  :config
  (defun ian/desktop-remove ()
    "Remove desktop file after loading."
    (let ((desktop desktop-dirname))
      (desktop-remove)
      (setq desktop-dirname desktop)))

  (defun ian/saved-desktop-p ()
    "Check if desktop exists."
    (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

  (defun ian/desktop-restore ()
    "Restore a saved emacs session."
    (interactive)
    (desktop-save-mode t)
    (if (ian/saved-desktop-p)
        (desktop-read)
      (message "No desktop found."))))

;; ============================================================================
;; 2. ALERT (Notification System)
;; ============================================================================

(use-package alert
  :custom
  (alert-default-style (if (eq system-type 'darwin)
                           'osx-notifier
                         'libnotify))
  (alert-log-messages t)
  :config
  ;; Telegram notifications (for telega)
  (add-to-list 'alert-user-configuration
               '(((:category . "telega"))
                 log nil))

  ;; Alert on mentions
  (add-to-list 'alert-user-configuration
               '(((:message . "@\\w+\\|your-nick-here")
                  (:category . "telega"))
                 libnotify nil)))

;; ============================================================================
;; 3. PROCED (Process Manager - htop-like)
;; ============================================================================

(use-package proced
  :straight (:type built-in)
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-enable-color-flag t)
  (proced-filter 'all))

;; ============================================================================
;; 4. SAVEHIST (Minibuffer History)
;; ============================================================================

(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-additional-variables
   '(search-ring regexp-search-ring kill-ring))
  (savehist-autosave-interval 60)
  (history-length 1000)
  (history-delete-duplicates t)
  :config
  (savehist-mode 1))

;; ============================================================================
;; 5. RECENTF (Recent Files)
;; ============================================================================

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

;; ============================================================================
;; 6. SAVEPLACE (Remember Cursor Position)
;; ============================================================================

(use-package saveplace
  :straight (:type built-in)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (save-place-mode 1))

(provide 'core-session)
;;; core-session.el ends here
