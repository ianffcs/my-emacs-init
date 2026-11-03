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

  ;; Clojure REPL access: the row is added to `ian/mcp-servers' when a
  ;; CIDER REPL connects.  Checking at config time can never see a REPL,
  ;; and a port captured early would go stale, so it is lazy per connection.
  (defvar cider-connected-hook)
  (add-hook 'cider-connected-hook #'ian/mcp-maybe-register-clojure)

  ;; gptel gets the same servers as tools on demand: M-x gptel-mcp-connect
  ;; (or mcp-hub-start-all-server) starts them explicitly.  Nothing spawns
  ;; npx/uvx processes at startup.
  )

;; ============================================================================
;; SHARED MCP SERVER DEFINITIONS
;; ============================================================================

;; Defined with `defvar' because `ian/mcp-set-server' updates rows at runtime.
(defvar ian/mcp-servers
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
  "The only source of local MCP servers, shared by mcp-hub (org-mcp and
gptel tools) and agent-shell sessions.  Each entry is (NAME :command CMD
:args ARGS [:env (\"KEY=VALUE\" ...)] [:roots t]); :roots marks the
filesystem server, which gets its roots from
`mcp-filesystem-server-project-root' or /tmp.  Change rows through
`ian/mcp-set-server' so both consumer lists are re-derived.")

(defvar mcp-hub-servers)
(defvar agent-shell-mcp-servers)
(defvar mcp-filesystem-server-project-root)

(defun ian/mcp-command (command)
  "Resolve COMMAND to an installed binary, falling back to COMMAND itself."
  (or (executable-find command) command))

(defun ian/mcp-env-alist (env)
  "Split \"K=V\" strings in ENV into an alist of (KEY . VALUE).
Only the first \"=\" separates; VALUE may itself contain \"=\"."
  (mapcar (lambda (pair)
            (if-let* ((i (string-search "=" pair)))
                (cons (substring pair 0 i) (substring pair (1+ i)))
              (cons pair "")))
          env))

(defun ian/mcp-filesystem-roots ()
  "Return the root directories for servers marked :roots.
This is the single place that reads `mcp-filesystem-server-project-root'."
  (if (and (boundp 'mcp-filesystem-server-project-root)
           (listp mcp-filesystem-server-project-root)
           mcp-filesystem-server-project-root)
      (mapcar #'expand-file-name mcp-filesystem-server-project-root)
    '("/tmp")))

(defun ian/mcp-normalize-server (server)
  "Normalise table row SERVER into a plist shared by every converter.
Keys: :name, :command (resolved), :args, :env (alist of KEY . VALUE) and
:roots (list of directories, or nil)."
  (pcase-let ((`(,name . ,plist) server))
    (list :name name
          :command (ian/mcp-command (plist-get plist :command))
          :args (plist-get plist :args)
          :env (ian/mcp-env-alist (plist-get plist :env))
          :roots (when (plist-get plist :roots)
                   (ian/mcp-filesystem-roots)))))

(defun ian/mcp-hub-server-defs ()
  "Return `ian/mcp-servers' in mcp-hub's plist format."
  (mapcar
   (lambda (server)
     (let ((n (ian/mcp-normalize-server server)))
       `(,(plist-get n :name)
         . (:command ,(plist-get n :command)
            :args ,(plist-get n :args)
            ,@(when-let* ((env (plist-get n :env)))
                (list :env (mapcan (lambda (kv)
                                     (list (intern (concat ":" (car kv)))
                                           (cdr kv)))
                                   env)))
            ,@(when-let* ((roots (plist-get n :roots)))
                (list :roots roots))))))
   ian/mcp-servers))

(defun ian/mcp-agent-shell-server-defs ()
  "Return `ian/mcp-servers' in agent-shell's ACP alist format.
ACP has no roots field, so roots are appended to the command's args,
which is how the filesystem server takes its allowed directories."
  (mapcar
   (lambda (server)
     (let ((n (ian/mcp-normalize-server server)))
       `((name . ,(plist-get n :name))
         (command . ,(plist-get n :command))
         (args . ,(append (plist-get n :args) (plist-get n :roots)))
         ,@(when-let* ((env (plist-get n :env)))
             (list `(env . ,(mapcar (lambda (kv)
                                      `((name . ,(car kv)) (value . ,(cdr kv))))
                                    env)))))))
   ian/mcp-servers))

(defun ian/mcp-sync-consumers ()
  "Re-derive both consumer server lists from `ian/mcp-servers'."
  (setq mcp-hub-servers (ian/mcp-hub-server-defs)
        agent-shell-mcp-servers (ian/mcp-agent-shell-server-defs)))

(defun ian/mcp-set-server (name &rest plist)
  "Add or replace the `ian/mcp-servers' row NAME with PLIST, then re-sync."
  (setq ian/mcp-servers
        (append (assoc-delete-all name (copy-sequence ian/mcp-servers) #'string=)
                (list (cons name plist))))
  (ian/mcp-sync-consumers))

(defun ian/mcp-maybe-register-clojure ()
  "Add or refresh the Clojure MCP server row for the current CIDER REPL."
  (when-let* ((port (and (fboundp 'cider-current-repl-port)
                         (ignore-errors (cider-current-repl-port)))))
    (ian/mcp-set-server "clojure"
                        :command "clojure"
                        :args (list "-X:mcp" "--port" (number-to-string port)))))

;; agent-shell sessions get the same MCP servers (tool-chat loads before
;; tool-mcp, so set this once agent-shell itself loads).
(with-eval-after-load 'agent-shell
  (setq agent-shell-mcp-servers (ian/mcp-agent-shell-server-defs)))

;; ============================================================================

(provide 'tool-mcp)
;;; tool-mcp.el ends here
