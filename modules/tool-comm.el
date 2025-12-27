;;; tool-comm.el --- Communication Tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Communication tools: Telega (Telegram), Circe (IRC), GNUS (Email), Elfeed (RSS).
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. TELEGA (Telegram Client)
;; ============================================================================

(use-package telega
  :commands telega
  :bind ("C-c T t" . telega)
  :custom
  (telega-use-images t)
  (telega-emoji-use-images nil)
  :config
  ;; Enable notifications
  (telega-notifications-mode 1)

  ;; Company completion for telega
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '(telega-company-emoji
                             telega-company-username
                             telega-company-hashtag)
                           (when (telega-chat-bot-p telega-chatbuf--chat)
                             '(telega-company-botcmd))))
              (company-mode 1))))

;; ============================================================================
;; 2. CIRCE (IRC Client)
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
  ;; Helper to fetch password from auth-source
  (defun ian/circe-fetch-password (&rest params)
    "Fetch the password for an IRC network."
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        (error "Password not found for %S" params))))

  (defun ian/circe-nickserv-password (server)
    "Fetch NickServ password for SERVER."
    (ian/circe-fetch-password :login "your-nick" :machine server))

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
;; 3. GNUS (Email Client)
;; ============================================================================

(use-package gnus
  :straight (:type built-in)
  :commands gnus
  :custom
  (gnus-select-method '(nnnil nil))
  (gnus-asynchronous t)
  (gnus-use-cache t)
  (gnus-use-header-prefetch t)
  :config
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
;; 4. ELFEED (RSS Reader)
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
;; 5. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/comm-menu ()
    "Communication commands"
    ["Apps"
     ("t" "Telega (Telegram)" telega)
     ("i" "IRC (Circe)" circe)
     ("g" "Gnus (Email)" gnus)
     ("r" "Elfeed (RSS)" elfeed)]
    ["Elfeed"
     ("u" "Update feeds" elfeed-update)
     ("s" "Search" elfeed-search-set-filter)])

  (global-set-key (kbd "C-c T T") #'ian/comm-menu))

(provide 'tool-comm)
;;; tool-comm.el ends here
