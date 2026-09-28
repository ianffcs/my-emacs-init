;;; tool-mcp.el --- Model Context Protocol tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Model Context Protocol tools.

;;; Code:


;; 13. MCP (Model Context Protocol)
;; ============================================================================

(defcustom ian/emacs-mcp-server-autostart nil
  "When non-nil, auto-start `emacs-mcp-server` after Emacs init."
  :type 'boolean
  :group 'ian)

(defcustom ian/org-mcp-autostart nil
  "When non-nil, auto-enable `org-mcp` and start `mcp-server-lib` after init."
  :type 'boolean
  :group 'ian)

(use-package mcp-server
  :straight (:type git :host github :repo "rhblind/emacs-mcp-server"
                   :files ("*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :commands (mcp-server-start-unix
             mcp-server-stop
             mcp-server-status
             mcp-server-restart
             mcp-server-get-socket-path)
  :custom
  (mcp-server-socket-name 'user)
  (mcp-server-debug nil)
  :config
  (when ian/emacs-mcp-server-autostart
    (add-hook 'emacs-startup-hook #'mcp-server-start-unix)))

(use-package mcp-server-lib
  :straight (:type git :host github :repo "laurynas-biveinis/mcp-server-lib.el")
  :commands (mcp-server-lib-install
             mcp-server-lib-start
             mcp-server-lib-stop
             mcp-server-lib-describe-setup
             mcp-server-lib-show-metrics))

(use-package org-mcp
  :straight (:type git :host github :repo "laurynas-biveinis/org-mcp")
  :after (org mcp-server-lib)
  :commands (org-mcp-enable org-mcp-disable)
  :custom
  ;; Keep this explicit and narrow; add files interactively as needed.
  (org-mcp-allowed-files (list (expand-file-name "inbox.org" org-directory))))

(defun ian/org-mcp-allow-current-file ()
  "Allow current Org file for org-mcp access."
  (interactive)
  (require 'org-mcp)
  (if-let* ((file (buffer-file-name))
            (is-org (string-match-p "\\.org\\'" file)))
      (progn
        (setq org-mcp-allowed-files
              (delete-dups
               (cons (expand-file-name file)
                     (mapcar #'expand-file-name org-mcp-allowed-files))))
        (message "org-mcp allowed files: %d" (length org-mcp-allowed-files)))
    (message "Current buffer is not a file-backed Org buffer")))

(defun ian/org-mcp-start ()
  "Enable org-mcp resources/tools and start mcp-server-lib."
  (interactive)
  (require 'org-mcp)
  (require 'mcp-server-lib)
  (org-mcp-enable)
  (mcp-server-lib-start)
  (message "org-mcp enabled and mcp-server-lib started"))

(defun ian/org-mcp-stop ()
  "Disable org-mcp resources/tools and stop mcp-server-lib."
  (interactive)
  (require 'org-mcp)
  (require 'mcp-server-lib)
  (org-mcp-disable)
  (mcp-server-lib-stop)
  (message "org-mcp disabled and mcp-server-lib stopped"))

(when ian/org-mcp-autostart
  (add-hook 'emacs-startup-hook #'ian/org-mcp-start))

(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el" :nonrecursive t)
  :after gptel
  :commands (mcp-hub mcp-hub-start-all-server gptel-mcp-connect gptel-mcp-disconnect)
  :config
  (require 'mcp-hub)

  (let ((filesystem-roots
         (if (and (boundp 'mcp-filesystem-server-project-root)
                  (listp mcp-filesystem-server-project-root)
                  mcp-filesystem-server-project-root)
             (mapcar #'expand-file-name mcp-filesystem-server-project-root)
           '("/tmp"))))
    (setq mcp-hub-servers
          `(;; Filesystem access
            ("filesystem" . (:command "npx"
                                      :args ("-y" "@modelcontextprotocol/server-filesystem")
                                      :roots ,filesystem-roots))

            ;; DuckDuckGo search
            ("duckduckgo" . (:command ,(or (executable-find "uvx") "uvx")
                                      :args ("duckduckgo-mcp-server")))

            ;; URL fetching
            ("fetch" . (:command ,(or (executable-find "uvx") "uvx")
                                 :args ("mcp-server-fetch")))

            ;; Shell commands (restricted)
            ("mcp-shell-server" . (:command ,(or (executable-find "uvx") "uvx")
                                            :args ("mcp-shell-server")
                                            :env (:ALLOW_COMMANDS
                                                  "bc,cat,chmod,curl,date,echo,find,git,grep,head,jq,ls,pwd,rg,sed,tail,wc")))

            ;; Clojure REPL (when CIDER is active)
            ,@(when (and (fboundp 'cider-current-repl)
                         (ignore-errors (cider-current-repl)))
                `(("clojure" . (:command "clojure"
                                         :args ("-X:mcp"
                                                "--port"
                                                ,(number-to-string
                                                  (cider-current-repl-port)))))))))))

;; ============================================================================

(provide 'tool-mcp)
;;; tool-mcp.el ends here
