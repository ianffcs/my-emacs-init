;;; core-auth.el --- Authentication & Security -*- lexical-binding: t; -*-

;;; Commentary:
;; Authentication sources, password management, and security.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. AUTH-SOURCES (GPG Integration)
;; ============================================================================

(setq auth-sources
      '((:source "~/.authinfo.gpg")
        (:source "~/.netrc")))

;; Debug auth-source if needed
;; (setq auth-source-debug t)

;; ============================================================================
;; 2. KEEPASS MODE
;; ============================================================================

(use-package keepass-mode
  :mode ("\\.kdbx\\'" . keepass-mode))

;; ============================================================================
;; 3. EPA/EPG (GnuPG Integration)
;; ============================================================================

(use-package epa
  :straight (:type built-in)
  :custom
  (epa-armor t)
  (epa-file-select-keys nil)
  :config
  (epa-file-enable))

(use-package epg
  :straight (:type built-in)
  :custom
  (epg-gpg-program "gpg2")
  (epg-pinentry-mode 'loopback))

;; ============================================================================
;; 4. ORG-CRYPT (Encrypt Org Headings)
;; ============================================================================

(use-package org-crypt
  :straight (:type built-in)
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key nil)) ;; Use symmetric encryption by default

;; ============================================================================
;; 5. PASS (Password Store)
;; ============================================================================

(use-package pass
  :commands pass)

(use-package password-store
  :commands (password-store-copy
             password-store-get
             password-store-insert
             password-store-generate))

(use-package auth-source-pass
  :straight (:type built-in)
  :config
  (auth-source-pass-enable))

;; ============================================================================
;; 6. PINENTRY (GPG PIN Entry)
;; ============================================================================

(use-package pinentry
  :if (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(provide 'core-auth)
;;; core-auth.el ends here
