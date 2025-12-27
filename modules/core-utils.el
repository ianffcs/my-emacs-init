;;; core-utils.el --- Essential utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility packages used across all modules: which-key, helpful, yasnippet, etc.
;;
;; NOTE: This file has been cleaned up to remove duplications:
;; - Password packages (pass, password-store, auth-source-pass) → core-auth.el
;; - ian/indent-buffer → core-editor.el (keybinding: C-c e i)
;;
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. WHICH-KEY (Keybinding Help)
;; ============================================================================

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-separator " → ")
  (which-key-prefix-prefix "+")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-allow-evil-operators t)
  :config
  (which-key-mode)

  ;; Custom prefixes
  (which-key-add-key-based-replacements
    "C-c g" "AI/GPT"
    "C-c o" "Org"
    "C-c p" "Project"
    "C-c n" "Notes/Roam"
    "C-c t" "Terminal/Toggle"
    "C-c l" "LSP"
    "C-c h" "Help/Dashboard"
    "C-c r" "Reveal"
    "C-c T" "TRAMP"
    "C-c M-" "Org-AI"
    "C-x r" "Registers/Bookmarks"
    "C-x n" "Narrow"))

;; ============================================================================
;; 2. HELPFUL (Better Help Buffers)
;; ============================================================================

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point))
  :custom
  (helpful-max-buffers 5))

;; ============================================================================
;; 3. YASNIPPET (Snippets)
;; ============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :bind (:map yas-minor-mode-map
              ("C-c y i" . yas-insert-snippet)
              ("C-c y n" . yas-new-snippet)
              ("C-c y v" . yas-visit-snippet-file)
              ("C-c y r" . yas-reload-all))
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :config
  ;; Create snippets directory if it doesn't exist
  (unless (file-exists-p (car yas-snippet-dirs))
    (make-directory (car yas-snippet-dirs) t)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind ("C-c y c" . consult-yasnippet))

;; ============================================================================
;; 4. TRANSIENT (Menus)
;; ============================================================================

(use-package transient
  :custom
  (transient-default-level 5)
  (transient-display-buffer-action '(display-buffer-below-selected)))

;; ============================================================================
;; 5. RESTART EMACS
;; ============================================================================

(use-package restart-emacs
  :commands restart-emacs
  :bind ("C-c q r" . restart-emacs))

;; ============================================================================
;; 6. DIMINISH (Hide minor modes from modeline)
;; ============================================================================

(use-package diminish
  :demand t)

;; ============================================================================
;; 7. GCMH (Garbage Collection Magic Hack)
;; ============================================================================

(use-package gcmh
  :diminish
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 64 1024 1024))  ; 64MB
  :config
  (gcmh-mode 1))

;; ============================================================================
;; 8. NO-LITTERING (Keep .emacs.d clean)
;; ============================================================================

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;; ============================================================================
;; 9. CRUX (Useful Interactive Commands)
;; ============================================================================

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("C-c R" . crux-rename-file-and-buffer)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("C-S-RET" . crux-smart-open-line-above)
         ("S-RET" . crux-smart-open-line)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)))

;; ============================================================================
;; 10. GENERAL UTILITY FUNCTIONS
;; ============================================================================

(defun ian/reload-init ()
  "Reload init.el."
  (interactive)
  (load-file user-init-file)
  (message "Reloaded init.el"))

(defun ian/open-init-file ()
  "Open init.el."
  (interactive)
  (find-file user-init-file))

(defun ian/sudo-save ()
  "Save the current buffer using sudo."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer not visiting a file")
    (write-file (concat "/sudo::" buffer-file-name))))

(defun ian/kill-all-buffers ()
  "Kill all buffers except *scratch* and *Messages*."
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if
         (lambda (buf)
           (member (buffer-name buf) '("*scratch*" "*Messages*")))
         (buffer-list)))
  (message "Killed all buffers"))

(defun ian/toggle-maximize-buffer ()
  "Maximize/restore current buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; Keybindings for utility functions
(global-set-key (kbd "C-c q i") #'ian/reload-init)
(global-set-key (kbd "C-c q I") #'ian/open-init-file)
(global-set-key (kbd "C-c q k") #'ian/kill-all-buffers)
(global-set-key (kbd "C-c q m") #'ian/toggle-maximize-buffer)

(provide 'core-utils)
;;; core-utils.el ends here
