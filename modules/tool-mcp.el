;;; tool-mcp.el --- Model Context Protocol tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Model Context Protocol tools.

;;; Code:

(require 'subr-x)
(require 'pcase)

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
  (mcp-server-debug nil))

;; Register before startup regardless of when the deferred package loads.
(when ian/emacs-mcp-server-autostart
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

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
  (org-mcp-allowed-files (list (ian/org-file 'inbox))))

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

  (setq mcp-hub-servers (ian/mcp-hub-server-defs))

  ;; Clojure REPL access: register when a CIDER REPL connects.  Checking
  ;; at config time can never see a REPL, and a port captured early would
  ;; go stale, so the entry is added lazily per connection.
  (defvar cider-connected-hook)
  (defun ian/mcp-maybe-register-clojure ()
    "Register the Clojure MCP server for the current CIDER REPL."
    (when-let* ((port (and (fboundp 'cider-current-repl-port)
                           (ignore-errors (cider-current-repl-port)))))
      (unless (assoc "clojure" mcp-hub-servers)
        (add-to-list 'mcp-hub-servers
                     `("clojure" . (:command "clojure"
                                             :args ("-X:mcp"
                                                    "--port"
                                                    ,(number-to-string port))))))))
  (add-hook 'cider-connected-hook #'ian/mcp-maybe-register-clojure)

  ;; gptel gets the same servers as tools on demand: M-x gptel-mcp-connect
  ;; (or mcp-hub-start-all-server) starts them explicitly.  Nothing spawns
  ;; npx/uvx processes at startup.
  )

;; ============================================================================
;; SHARED MCP SERVER DEFINITIONS
;; ============================================================================

(defconst ian/mcp-servers
  '(("filesystem" :command "npx"
     :args ("-y" "@modelcontextprotocol/server-filesystem")
     :roots t)
    ("duckduckgo" :command "uvx"
     :args ("duckduckgo-mcp-server"))
    ("fetch" :command "uvx"
     :args ("mcp-server-fetch"))
    ("mcp-shell-server" :command "uvx"
     :args ("mcp-shell-server")
     :env ("ALLOW_COMMANDS=bc,cat,chmod,curl,date,echo,find,git,grep,head,jq,ls,pwd,rg,sed,tail,wc")))
  "Local MCP servers shared by mcp-hub (org-mcp and gptel tools) and
agent-shell sessions.  Each entry is (NAME :command CMD :args ARGS
[:env (\"KEY=VALUE\" ...)] [:roots t]); :roots marks the filesystem
server, which gets its roots from `mcp-filesystem-server-project-root'
or /tmp.")

(defun ian/mcp-command (command)
  "Resolve COMMAND to an installed binary, falling back to COMMAND itself."
  (or (executable-find command) command))

(defun ian/mcp-env-alist (env)
  "Split \"K=V\" strings in ENV into an alist of (KEY . VALUE)."
  (mapcar (lambda (pair)
            (cons (car (split-string pair "="))
                  (cadr (split-string pair "=" t))))
          env))

(defun ian/mcp-hub-server-defs ()
  "Return `ian/mcp-servers' in mcp-hub's plist format."
  (mapcar
   (lambda (server)
     (pcase-let ((`(,name . ,plist) server))
       `(,name
         . (:command ,(ian/mcp-command (plist-get plist :command))
            :args ,(plist-get plist :args)
            ,@(when-let* ((env (ian/mcp-env-alist (plist-get plist :env))))
                (list :env (apply #'append
                                  (mapcar (lambda (kv)
                                            (list (intern (concat ":" (car kv)))
                                                  (cdr kv)))
                                          env))))
            ,@(when (plist-get plist :roots)
                (list :roots
                      (if (and (boundp 'mcp-filesystem-server-project-root)
                               (listp mcp-filesystem-server-project-root)
                               mcp-filesystem-server-project-root)
                          (mapcar #'expand-file-name mcp-filesystem-server-project-root)
                        '("/tmp"))))))))
   ian/mcp-servers))

(defun ian/mcp-agent-shell-server-defs ()
  "Return `ian/mcp-servers' in agent-shell's ACP alist format."
  (mapcar
   (lambda (server)
     (pcase-let ((`(,name . ,plist) server))
       `((name . ,name)
         (command . ,(ian/mcp-command (plist-get plist :command)))
         (args . ,(plist-get plist :args))
         ,@(when-let* ((env (ian/mcp-env-alist (plist-get plist :env))))
             (list `(env . ,(mapcar (lambda (kv)
                                      `((name . ,(car kv)) (value . ,(cdr kv))))
                                    env)))))))
   ian/mcp-servers))

;; agent-shell sessions get the same MCP servers (tool-chat loads before
;; tool-mcp, so set this once agent-shell itself loads).
(with-eval-after-load 'agent-shell
  (setq agent-shell-mcp-servers (ian/mcp-agent-shell-server-defs)))

;; ============================================================================

(provide 'tool-mcp)
;;; tool-mcp.el ends here
