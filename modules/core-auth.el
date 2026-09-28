;;; core-auth.el --- Authentication & Security -*- lexical-binding: t; -*-

;;; Commentary:
;; Authentication sources, password management, and security.
;; Migrated from README.org literate config.

;;; Code:

(defvar org-tags-exclude-from-inheritance)

;; ============================================================================
;; 1. AUTH-SOURCES (GPG Integration)
;; ============================================================================

(setq auth-sources
      '((:source "~/.authinfo.gpg")
        (:source "~/.netrc")
        ;; Emacs' built-in Secret Service backend also works with KeePassXC.
        ;; Keep it last so existing authinfo/netrc entries retain precedence.
        "secrets:Login"))

;; Debug auth-source if needed
;; (setq auth-source-debug t)

;; Shared credential lookup for AI providers and communication tools.
(require 'auth-source)
(require 'subr-x)

(defconst ian/ai-host-env-map
  '(("api.openai.com" . "OPENAI_API_KEY")
    ("generativelanguage.googleapis.com" . "GEMINI_API_KEY")
    ("api.anthropic.com" . "ANTHROPIC_API_KEY"))
  "Mapping from API host to environment variable fallback for API keys.")

(defconst ian/ai-host-aliases
  '(("api.anthropic.com" . ("api.claude.ai")))
  "Host aliases accepted when resolving API keys from auth-source.")

(defun ian/authinfo-secret (host &optional user)
  "Return secret for HOST from auth-source. Default USER is \"apikey\"."
  (let* ((user (or user "apikey"))
         (entry (car (auth-source-search
                      :host host
                      :user user
                      :require '(:secret))))
         (secret (auth-info-password entry)))
    secret))

(defun ian/get-key (host &optional noerror)
  "Get API key for HOST from auth-source, then env var fallback.
When NOERROR is non-nil, return nil instead of signaling an error."
  (let* ((aliases (alist-get host ian/ai-host-aliases nil nil #'string=))
         (hosts (cons host aliases))
         (auth-key (catch 'found
                     (dolist (candidate hosts)
                       (when-let* ((secret (ian/authinfo-secret candidate)))
                         (unless (string-empty-p secret)
                           (throw 'found secret))))
                     nil))
         (env-var (alist-get host ian/ai-host-env-map nil nil #'string=))
         (env-key (and env-var (getenv env-var)))
         (key (or auth-key env-key)))
    (cond
     ((and (stringp key) (not (string-empty-p key))) key)
     (noerror nil)
     (t
      (error (concat "Missing API key for %s. Add one in ~/.authinfo.gpg as "
                     "\"machine %s login apikey password <KEY>\" "
                     "or set env var %s")
             host host (or env-var "YOUR_API_KEY_ENV"))))))


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
  :after auth-source
  :config
  (auth-source-pass-enable))

;; ============================================================================
;; 6. PINENTRY (GPG PIN Entry)
;; ============================================================================

(use-package pinentry
  :if (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  :after epa
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(provide 'core-auth)
;;; core-auth.el ends here
