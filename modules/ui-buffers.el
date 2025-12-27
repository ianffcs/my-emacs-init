;;; ui-buffers.el --- Buffer management -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer listing, organization, and management.
;;
;; NOTE: This file has been cleaned up to remove duplications:
;; - autorevert → core-editor.el §17
;;
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. IBUFFER (Better Buffer List)
;; ============================================================================

(use-package ibuffer
  :straight (:type built-in)
  :hook (ibuffer-mode . hl-line-mode)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-use-other-window nil)
  (ibuffer-movement-cycle t)
  (ibuffer-default-sorting-mode 'filename/process)
  :config
  ;; Custom filter groups
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Org" (or (mode . org-mode)
                      (mode . org-agenda-mode)))
           ("Magit" (or (name . "^magit")
                        (mode . magit-mode)))
           ("Code" (or (derived-mode . prog-mode)
                       (mode . ess-mode)
                       (mode . compilation-mode)))
           ("Clojure" (or (mode . clojure-mode)
                          (mode . clojurescript-mode)
                          (mode . cider-repl-mode)))
           ("Python" (or (mode . python-mode)
                         (mode . python-ts-mode)
                         (mode . inferior-python-mode)))
           ("Web" (or (mode . web-mode)
                      (mode . js-mode)
                      (mode . typescript-mode)
                      (mode . css-mode)
                      (mode . html-mode)))
           ("Text" (and (derived-mode . text-mode)
                        (not (name . "^\\*"))))
           ("Terminal" (or (mode . vterm-mode)
                           (mode . eshell-mode)
                           (mode . term-mode)
                           (mode . eat-mode)))
           ("Chat" (or (mode . gptel-mode)
                       (name . "^\\*ChatGPT\\*")
                       (name . "^\\*Claude\\*")))
           ("Help" (or (name . "^\\*Help\\*$")
                       (name . "^\\*helpful")
                       (name . "^\\*Apropos\\*$")
                       (name . "^\\*info\\*$")))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*Backtrace\\*$"))))))

  ;; Auto-switch to filter groups
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))

  ;; Don't show empty groups
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Column widths
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

(use-package ibuffer-tramp
  :after ibuffer)

(use-package ibuffer-sidebar
  :commands ibuffer-sidebar-toggle-sidebar
  :custom
  (ibuffer-sidebar-use-custom-font t))

;; Ibuffer with Projectile groups
(use-package ibuffer-projectile
  :after (ibuffer projectile)
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix "Project: "))

;; Ibuffer with VC status
(use-package ibuffer-vc
  :after ibuffer
  :commands ibuffer-vc-set-filter-groups-by-vc-root)

;; ============================================================================
;; 2. UNIQUIFY (Unique Buffer Names)
;; ============================================================================

(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; ============================================================================
;; 3. MIDNIGHT (Auto-cleanup Old Buffers)
;; ============================================================================

(use-package midnight
  :straight (:type built-in)
  :custom
  (clean-buffer-list-delay-general 2)  ; Days
  (clean-buffer-list-delay-special (* 4 3600))  ; 4 hours for special buffers
  :config
  ;; Buffers to never kill
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "*scratch*")
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "*Messages*")
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "*dashboard*")

  ;; Patterns for buffers to never kill
  (add-to-list 'clean-buffer-list-kill-never-regexps "^\\*EGLOT")
  (add-to-list 'clean-buffer-list-kill-never-regexps "^\\*cider")
  (add-to-list 'clean-buffer-list-kill-never-regexps "^\\*vterm")

  (midnight-mode 1))

(use-package vlf
  :defer t
  :config
  (require 'vlf-setup))

;; ============================================================================
;; 4. BUFFER HELPER FUNCTIONS
;; ============================================================================

(defun ian/kill-current-buffer ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ian/kill-other-buffers ()
  "Kill all buffers except the current one and special buffers."
  (interactive)
  (let ((keep-buffers '("*scratch*" "*Messages*" "*dashboard*")))
    (dolist (buffer (buffer-list))
      (unless (or (eq buffer (current-buffer))
                  (member (buffer-name buffer) keep-buffers)
                  (get-buffer-process buffer))
        (kill-buffer buffer))))
  (message "Killed other buffers"))

(defun ian/switch-to-scratch ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun ian/switch-to-messages ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun ian/new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buf))

(defun ian/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "Buffer reverted"))

;; Keybindings
(global-set-key (kbd "C-x k") #'ian/kill-current-buffer)
(global-set-key (kbd "C-c b k") #'ian/kill-other-buffers)
(global-set-key (kbd "C-c b s") #'ian/switch-to-scratch)
(global-set-key (kbd "C-c b m") #'ian/switch-to-messages)
(global-set-key (kbd "C-c b n") #'ian/new-empty-buffer)
(global-set-key (kbd "C-c b r") #'ian/revert-buffer-no-confirm)

;; ============================================================================
;; 5. BUFFER TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/buffer-menu ()
                           "Buffer management commands"
                           ["Switch"
                            ("b" "Switch buffer" switch-to-buffer)
                            ("B" "Switch other" switch-to-buffer-other-window)
                            ("s" "Scratch" ian/switch-to-scratch)
                            ("m" "Messages" ian/switch-to-messages)
                            ("n" "New buffer" ian/new-empty-buffer)]
                           ["Kill"
                            ("k" "Kill current" ian/kill-current-buffer)
                            ("K" "Kill others" ian/kill-other-buffers)
                            ("x" "Kill buffer" kill-buffer)]
                           ["List"
                            ("l" "List (ibuffer)" ibuffer)
                            ("r" "Revert" ian/revert-buffer-no-confirm)
                            ("R" "Rename" rename-buffer)])

  (global-set-key (kbd "C-c b") #'ian/buffer-menu))

(provide 'ui-buffers)
;;; ui-buffers.el ends here
