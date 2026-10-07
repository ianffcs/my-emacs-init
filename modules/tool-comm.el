;;; tool-comm.el --- Communication Tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Communication tools: Telega, WhatsAppel, Circe, Gnus, and Elfeed.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. TELEGA (Telegram Client)
;; ============================================================================

(defvar ian/telega-tdlib-prefix
  (cond
   ((file-directory-p "~/.local/opt/tdlib-head")
    (expand-file-name "~/.local/opt/tdlib-head"))
   ((and (eq system-type 'gnu/linux)
         (file-exists-p "/usr/include/td/telegram/td_json_client.h"))
    "/usr")
   ((eq system-type 'darwin)
    (cond
     ((file-directory-p "/opt/homebrew/opt/tdlib")
      "/opt/homebrew/opt/tdlib")
     ((file-directory-p "/usr/local/opt/tdlib")
      "/usr/local/opt/tdlib"))))
  "TDLib installation prefix for telega.")

(use-package telega
  :commands telega
  :bind ("C-c T t" . telega)
  :init
  (when ian/telega-tdlib-prefix
    (setq telega-server-libs-prefix ian/telega-tdlib-prefix))
  :custom
  (telega-use-images t)
  (telega-emoji-use-images nil)
  :config
  ;; Enable notifications
  (telega-notifications-mode 1)

  ;; Cape-backed completion in telega chat buffers (no company needed)
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'telega-completion-at-point
                                #'cape-emoji)))))

;; ============================================================================
;; 2. WHATSAPPEL (WhatsApp Client)
;; ============================================================================

(use-package whatsapp
  :straight (:type git :host codeberg :repo "berkeley/whatsappel"
                   :files ("whatsapp.el" "whatsapp-profiles.el"
                           "whatsapp-delivery.el" "whatsapp-org.el"
                           ("scripts" "scripts/read-worker.py"
                            "scripts/send-worker.py" "scripts/media-worker.py"
                            "scripts/profile-worker.py" "scripts/bridge_protocol.py")))
  :commands (whatsapp-launch whatsapp whatsapp-connect whatsapp-qr)
  :bind ("C-c T w" . whatsapp-launch)
  :config
  ;; Resolve credentials only when the WhatsApp client is first used.
  (unless whatsapp-bridge-token
    (setq whatsapp-bridge-token
          (ian/authinfo-secret "127.0.0.1" "whatsappel"))))

;; ============================================================================
;; 3. CIRCE (IRC Client)
;; ============================================================================

(use-package circe
  :commands circe
  :bind ("C-c T i" . circe)
  :custom
  (circe-default-part-message nil)
  (circe-default-quit-message nil)
  (circe-format-say (format "{nick:+%ss}: {body}" 8))
  (circe-reduce-lurker-spam t)
  (circe-use-cycle-completion t)
  (lui-flyspell-p t)
  :config
  ;; Keep the general Circe lookup available for existing configurations.
  (defun ian/circe-fetch-password (&rest params)
    "Fetch the password for an IRC network."
    (require 'auth-source)
    (let ((match (car (apply #'auth-source-search params))))
      (if match
          (auth-info-password match)
        (error "Password not found for %S" params))))

  ;; Reuse the shared auth-source resolver for IRC credentials.
  (defun ian/circe-nickserv-password (server)
    "Fetch NickServ password for SERVER."
    (ian/circe-fetch-password :host server :user "your-nick" :require '(:secret)))

  ;; Count nicks in channel
  (defun ian/circe-count-nicks ()
    "Display the number of users on the current channel."
    (interactive)
    (when (eq major-mode 'circe-channel-mode)
      (message "%i users are online on %s."
               (length (circe-channel-nicks)) (buffer-name))))

  ;; Network configuration
  (setq circe-network-options
        '(("Libera Chat"
           :host "irc.libera.chat"
           :nick "your-nick"
           :tls t
           :port 6697
           :server-buffer-name "⇄ Libera Chat"
           :channels (:after-auth "#emacs" "#clojure"))
          ("OFTC"
           :host "irc.oftc.net"
           :nick "your-nick"
           :tls t
           :port 6697
           :server-buffer-name "⇄ OFTC"
           :channels (:after-auth "#debian"))))

  ;; Enable extra features
  (circe-lagmon-mode)
  (enable-circe-color-nicks)
  (enable-circe-display-images))

;; Circe notifications
(use-package circe-notifications
  :after circe
  :hook (circe-server-connected . enable-circe-notifications))

;; ============================================================================
;; 4. GNUS (Email Client)
;; ============================================================================

(defconst ian/outlook-mail-address "d.ian.b@live.com"
  "Mailbox configured for the Outlook account used by dev-machine.")

(defconst ian/outlook-imap-host "outlook.office365.com"
  "Outlook IMAP endpoint.")

(defconst ian/outlook-smtp-host "smtp.office365.com"
  "Outlook SMTP submission endpoint.")

(use-package auth-source-xoauth2-plugin
  :demand t
  :init
  ;; OAuth refresh tokens belong in oauth2.plstore.  Do not let nnimap's
  ;; create/save path try to write short-lived access tokens into authinfo.
  (setq auth-source-save-behavior nil)
  :config
  (auth-source-xoauth2-plugin-mode 1))

(use-package plstore
  :straight (:type built-in)
  :demand t
  :config
  ;; Use the EdDSA key for plstore encryption (principal email key).
  (setq plstore-encrypt-to "0476ED26015BD774ECFF61F4A5C40CA014A688C9"
        plstore-select-keys 'silent))

(use-package smtpmail
  :straight (:type built-in)
  :custom
  (message-send-mail-function #'smtpmail-send-it)
  (smtpmail-smtp-server ian/outlook-smtp-host)
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls))

(use-package gnus
  :straight (:type built-in)
  :commands gnus
  :config
  (setq gnus-select-method '(nnnil nil)
        gnus-asynchronous t
        gnus-use-cache t
        gnus-use-header-prefetch t)
  ;; Outlook OAuth2 credentials are resolved from ~/.authinfo.gpg.  The
  ;; matching entry should use auth=xoauth2 and
  ;; auth-source-xoauth2-predefined-service=microsoft; no password is stored.
  ;; Add these entries once with your preferred encrypted auth-source editor:
  ;; machine outlook.office365.com login d.ian.b@live.com port imaps auth xoauth2 auth-source-xoauth2-predefined-service microsoft
  ;; machine smtp.office365.com login d.ian.b@live.com port 587 auth xoauth2 auth-source-xoauth2-predefined-service microsoft
  (setq gnus-secondary-select-methods
        `((nnimap "Outlook"
           (nnimap-address ,ian/outlook-imap-host)
           (nnimap-server-port 993)
           (nnimap-stream ssl)
           (nnimap-authenticator xoauth2)
           (nnimap-user ,ian/outlook-mail-address)
           (nnir-search-engine imap))))

  ;; Posting styles
  (setq gnus-posting-styles
        `((".*"
           (address ,ian/outlook-mail-address))))

  ;; Recreate oauth2.plstore if missing (after GPG key rotation).
  (defun ian/ensure-oauth2-plstore ()
    "Initialize oauth2.plstore if missing by opening the plstore connection."
    (let ((plstore-file (expand-file-name "oauth2.plstore" user-emacs-directory)))
      (unless (file-exists-p plstore-file)
        (require 'plstore)
        (let ((store (plstore-open plstore-file)))
          (plstore-close store)))))

  ;; Unlock GPG key once per session when auth-source first needs it.
  (defun ian/unlock-authinfo-gpg ()
    "Unlock .authinfo.gpg once per session by accessing auth-source."
    (require 'auth-source)
    (when (file-exists-p (expand-file-name ".authinfo.gpg" (getenv "HOME")))
      (auth-source-search :host "openrouter.ai" :max 1)))

  (add-hook 'gnus-startup-hook #'ian/ensure-oauth2-plstore)
  (add-hook 'gnus-startup-hook #'ian/unlock-authinfo-gpg)
  )

;; ============================================================================
;; 5. ELFEED (RSS Reader)
;; ============================================================================

(use-package elfeed
  :commands elfeed
  :bind (("C-c T r" . elfeed)
         :map elfeed-search-mode-map
         ("q" . elfeed-save-db-and-bury)
         ("Q" . elfeed-save-db-and-bury)
         ("m" . elfeed-toggle-star)
         ("U" . elfeed-update)
         :map elfeed-show-mode-map
         ("C-<right>" . elfeed-show-next)
         ("C-<left>" . elfeed-show-prev)
         ("n" . elfeed-show-next)
         ("p" . elfeed-show-prev))
  :custom
  (elfeed-search-filter "@3-days-ago +unread")
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  :config
  (defun elfeed-save-db-and-bury ()
    "Save the elfeed db and bury the buffer."
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defun elfeed-toggle-star ()
    "Toggle star tag for current entry."
    (interactive)
    (elfeed-search-toggle-all 'star)))

;; Elfeed with Org configuration
(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (ian/org-file 'elfeed)))
  :config
  (elfeed-org))

;; Elfeed UI enhancements
(use-package elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup))

;; ============================================================================
;; 6. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/comm-menu ()
    "Communication commands"
    ["Apps"
     ("t" "Telega (Telegram)" telega)
     ("w" "WhatsAppel" whatsapp-launch)
     ("i" "IRC (Circe)" circe)
     ("g" "Gnus (Email)" gnus)
     ("r" "Elfeed (RSS)" elfeed)]
    ["Elfeed"
     ("u" "Update feeds" elfeed-update)
     ("s" "Search" elfeed-search-set-filter)])

  (global-set-key (kbd "C-c T T") #'ian/comm-menu))

(provide 'tool-comm)
;;; tool-comm.el ends here
