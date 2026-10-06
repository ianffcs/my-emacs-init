;;; core-settings.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Fundamental Emacs behavioral settings that don't fit elsewhere.
;;
;; NOTE: This file has been cleaned up to remove duplications:
;; - UI settings (GUI, frame, cursor, fringe, line numbers) → core-ui.el
;; - Editor settings (indentation, scrolling, undo, parens) → core-editor.el
;; - Session settings (savehist, recentf, saveplace, desktop) → core-session.el
;; - Encoding settings → core-os.el
;; - macOS settings → core-os.el
;; - Native compilation → early-init.el
;;
;; Migrated from README.org literate config.

;;; Code:

;; Set the shared Org root before tool and UI modules derive paths from it.
(defvar org-directory (expand-file-name "org" "~")
  "Root directory for Org files.")

;; ----------------------------------------------------------------------------
;; Org locations: named Org files under `org-directory'
;; ----------------------------------------------------------------------------

(defvar ian/org-files
  '((inbox . "inbox.org")
    (todo . "todo.org")
    (notes . "notes.org")
    (diary . "diary.org")
    (elfeed . "elfeed.org"))
  "Alist mapping a symbol to an Org file name under `org-directory'.
Reach these through `ian/org-file' instead of spelling the file names.")

(defun ian/org-file (name)
  "Return the absolute path of the Org file NAME under `org-directory'.
NAME is a key of `ian/org-files'.  Signal an error for an unknown NAME.
Resolved at call time, so a changed `org-directory' is honoured."
  (let ((entry (assq name ian/org-files)))
    (unless entry
      (error "Unknown Org file: %S" name))
    (expand-file-name (cdr entry) org-directory)))

(defun ian/org-existing-paths (paths)
  "Return the existing members of PATHS, expanded; drop the rest.
Both files and directories count as existing.  Keeps missing paths out of
`org-agenda-files' so Org never prompts for them interactively."
  (delq nil
        (mapcar (lambda (path)
                  (let ((expanded (expand-file-name path)))
                    (when (or (file-directory-p expanded)
                              (file-exists-p expanded))
                      expanded)))
                paths)))

;; ============================================================================
;; 1. PERFORMANCE
;; ============================================================================

;; Disable bidirectional text scanning for performance
;; (Most users don't need right-to-left language support)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; ============================================================================
;; 2. FILE HANDLING
;; ============================================================================

;; Don't create lock files (.#filename)
(setq create-lockfiles nil)

;; Large file warning threshold (100MB)
(setq large-file-warning-threshold (* 100 1024 1024))

;; ============================================================================
;; 3. MINIBUFFER & COMMANDS
;; ============================================================================

;; Allow nested minibuffer commands
(setq enable-recursive-minibuffers t)

;; Enable all disabled commands (downcase-region, upcase-region, etc.)
(setq disabled-command-function nil)

;; Kill process buffers without confirmation
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; ============================================================================
;; 4. CUSTOM FILE
;; ============================================================================

;; Store customizations in separate file (don't pollute init.el)
(setq custom-file (expand-file-name "custom/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; ============================================================================
;; 5. AUTO MODE ASSOCIATIONS
;; ============================================================================

(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sface\\'" . web-mode))
(add-to-list 'auto-mode-alist '("template\\.yaml\\'" . cfn-mode))
(add-to-list 'auto-mode-alist '("samconfig\\.toml\\'" . toml-mode))

(provide 'core-settings)
;;; core-settings.el ends here
