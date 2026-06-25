;;; core-packages.el --- Package Management (Straight.el)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package management and bootstrap configuration using straight.el
;; Migrated from README.org literate config

;;; Code:

;; 1. USER INFO (from Personal Keymap section)
(setq user-full-name "Ian Fernandez"
      user-mail-address "d.ian.b@live.com")

;; 3. STARTUP MESSAGE (from Beginning section)
(defvar current-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Let the coding begin! Be patient, %s!" current-user)

;; 4. PERFORMANCE ADJUSTMENTS (from Performance adjustments section)
(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024 8))  ; 8MB for LSP performance
(setq inhibit-compacting-font-caches t)

;; Defer font-lock while typing, apply on idle — eliminates lag in large files
(setq jit-lock-stealth-time 1.0
      jit-lock-defer-time 0.1
      jit-lock-stealth-nice 0.1)

;; 5. NATIVE COMPILATION TWEAKS
;; PATH/LIBRARY_PATH for Homebrew already set in early-init.el.

;; Suppress spurious native-compiler warnings from broken straight.el symlinks
;; (built on a different machine path). Run M-x straight-rebuild-all to fix permanently.
(with-eval-after-load 'comp
  (setq native-comp-async-report-warnings-errors 'silent))

;; 6. STRAIGHT.EL PERFORMANCE SETTINGS
(setq straight-check-for-modifications '(find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

;; 7. BOOTSTRAP STRAIGHT.EL
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 8. CONFIGURE USE-PACKAGE
(straight-use-package 'use-package)
(straight-use-package 'transient)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq use-package-compute-statistics nil)

;; no-littering: redirect auto-save/backup before any other package runs
(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;; Startup profiler
(use-package esup
  :commands esup)

;; Auto-install and remap tree-sitter grammars
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

;; Async operations (from Async section)
(use-package async
  :hook (after-init . (lambda ()
                        (setq async-bytecomp-allowed-packages '(all))
                        (dired-async-mode 1)
                        (async-bytecomp-package-mode 1))))

;; 9. FORCE BUILT-IN PACKAGES
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))
(straight-use-package '(org :type built-in))

;; 10. CUSTOM FILE SETUP (from Custom section)
(unless (file-exists-p (concat user-emacs-directory "custom/"))
  (make-directory (concat user-emacs-directory "custom/")))

(unless (file-exists-p (concat user-emacs-directory "custom/custom.el"))
  (make-empty-file (concat user-emacs-directory "custom/custom.el")))

;; 11. PRIVATE DIRECTORY SETUP (from Private section)
(defconst ian/private-dir (expand-file-name "private" user-emacs-directory))
(defconst ian/temp-dir (expand-file-name "cache" ian/private-dir))

(unless (file-exists-p ian/private-dir)
  (make-directory ian/private-dir :parents))

(unless (file-exists-p ian/temp-dir)
  (make-directory ian/temp-dir :parents))

;; 12. SYMLINK HANDLING
(setq vc-follow-symlinks t)

(provide 'core-packages)
;;; core-packages.el ends here
