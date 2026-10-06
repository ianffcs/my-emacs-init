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

;; One row per AI provider.  :host is the auth-source machine name (user
;; "apikey"), :aliases are further hosts accepted there, :env is the
;; environment variable fallback.  Provider symbols equal org-ai's service
;; symbols.
(defconst ian/ai-providers
  '((openai :host "api.openai.com" :env "OPENAI_API_KEY")
    (anthropic :host "api.anthropic.com" :aliases ("api.claude.ai")
               :env "ANTHROPIC_API_KEY")
    (google :host "generativelanguage.googleapis.com" :env "GEMINI_API_KEY"))
  "AI provider facts, keyed by provider symbol.
Each row is (PROVIDER :host HOST [:aliases HOSTS] :env ENV-VAR).")

(defconst ian/ai-completion-order
  '((openai . openai)
    (claude . anthropic))
  "Credential preference order for AI completion providers.
Each entry is (MINUET-PROVIDER . PROVIDER); the provider's key gates it.")

(defun ian/authinfo-secret (host &optional user)
  "Return secret for HOST from auth-source. Default USER is \"apikey\"."
  (let* ((user (or user "apikey"))
         (entry (car (auth-source-search
                      :host host
                      :user user
                      :require '(:secret))))
         (secret (auth-info-password entry)))
    secret))

(defun ian/ai-provider--row (provider)
  "Return the `ian/ai-providers' plist for PROVIDER, or signal an error."
  (or (alist-get provider ian/ai-providers)
      (error "Unknown AI provider: %S" provider)))

(defun ian/ai-key (provider &optional noerror)
  "Get API key for PROVIDER from auth-source, then env var fallback.
PROVIDER is a symbol in `ian/ai-providers'; an unknown one always signals.
When NOERROR is non-nil, return nil instead of signaling a missing-key error."
  (let* ((row (ian/ai-provider--row provider))
         (host (plist-get row :host))
         (env-var (plist-get row :env))
         (auth-key (catch 'found
                     (dolist (candidate (cons host (plist-get row :aliases)))
                       (when-let* ((secret (ian/authinfo-secret candidate)))
                         (unless (string-empty-p secret)
                           (throw 'found secret))))
                     nil))
         (env-key (and env-var (getenv env-var)))
         (key (or auth-key env-key)))
    (cond
     ((and (stringp key) (not (string-empty-p key))) key)
     (noerror nil)
     (t
      (error (concat "Missing API key for %s. Add one in ~/.authinfo.gpg as "
                     "\"machine %s login apikey password <KEY>\" "
                     "or set env var %s")
             provider host (or env-var "YOUR_API_KEY_ENV"))))))

(defun ian/ai-key-callback (provider)
  "Return a closure resolving the API key for PROVIDER at request time.
Use as a :key or :api-key callback so credentials are looked up when a
request is made, not while a package is being configured."
  (ian/ai-provider--row provider)
  (lambda () (ian/ai-key provider)))

(defun ian/ai-available-provider ()
  "Return the first minuet provider in `ian/ai-completion-order' with a key.
Fall back to 'openai-compatible (Ollama) when no hosted key is available."
  (or (catch 'found
        (dolist (entry ian/ai-completion-order)
          (when (ian/ai-key (cdr entry) t)
            (throw 'found (car entry)))))
      'openai-compatible))

(defun ian/get-host (name &optional noerror)
  "Get host entry NAME (a \"host:port\" string) from auth-source.
Looks for \"machine NAME login host password <host:port>\" in the auth
sources.  When NOERROR is non-nil, return nil instead of signaling an
error."
  (let ((value (ian/authinfo-secret name "host")))
    (cond
      ((and (stringp value) (not (string-empty-p value))) value)
      (noerror nil)
      (t
       (error (concat "Missing host entry for %s. Add one in ~/.authinfo.gpg as "
                      "\"machine %s login host password <HOST:PORT>\"")
              name name)))))

(defvar ian/ollama-host--value nil
  "Memoized result of `ian/ollama-host'.")

(defun ian/ollama-host ()
  "Return the Ollama host and port (\"host:port\", no URL scheme).
Resolved from auth-source: add \"machine ollama login host password
<host:port>\" to ~/.authinfo.gpg.  Set `IAN_OLLAMA_HOST' as a fallback
for machine-specific or remote Ollama instances, for example
\"spark.local:11434\".  The result is memoized for the session."
  (or ian/ollama-host--value
      (setq ian/ollama-host--value
            (or (ignore-errors (ian/get-host "ollama" t))
                (getenv "IAN_OLLAMA_HOST")))))

(defun ian/ollama-host-and-port ()
  "Return (HOST . PORT) parsed from `ian/ollama-host'."
  (let ((parts (split-string (ian/ollama-host) ":")))
    (cons (car parts) (string-to-number (cadr parts)))))


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
