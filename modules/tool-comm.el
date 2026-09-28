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
    (let ((match (car (apply 'auth-source-search params))))
      (if match
          (auth-info-password match)
        (error "Password not found for %S" params))))

  ;; Reuse the shared auth-source resolver for IRC credentials.
  (defun ian/circe-nickserv-password (server)
    "Fetch NickServ password for SERVER."
    (ian/authinfo-secret server "your-nick"))

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

(use-package auth-source-xoauth2-plugin
  :demand t
  :config
  (auth-source-xoauth2-plugin-mode 1))

(use-package gnus
  :straight (:type built-in)
  :commands gnus
  :config
  (setq gnus-select-method '(nnnil nil)
        gnus-asynchronous t
        gnus-use-cache t
        gnus-use-header-prefetch t)
  ;; Secondary select methods - configure with your email
  ;; Example for IMAP:
  ;; (setq gnus-secondary-select-methods
  ;;       '((nnimap "Gmail"
  ;;          (nnimap-address "imap.gmail.com")
  ;;          (nnimap-server-port 993)
  ;;          (nnimap-stream ssl)
  ;;          (nnir-search-engine imap)
  ;;          (nnimap-authinfo-file "~/.authinfo.gpg"))))

  ;; Posting styles
  ;; (setq gnus-posting-styles
  ;;       '((".*"
  ;;          (address "your-email@example.com")
  ;;          (signature "Your Name"))))
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
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
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
