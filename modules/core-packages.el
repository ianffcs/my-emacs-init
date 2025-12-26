;;; core-packages.el --- Package Management (Straight.el)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package management and bootstrap configuration using straight.el
;; Migrated from README.org literate config

;;; Code:

;; 1. COMPATIBILITY HACKS
(defvar thisfile nil "Workaround for magit compatibility")
(defvar symbol nil "Workaround for symbol compatibility")
(defvar value nil "Workaround for value compatibility")

;; 2. USER INFO (from Personal Keymap section)
(setq user-full-name "Ian Fernandez"
      user-mail-address "d.ian.b@live.com")

;; 3. STARTUP MESSAGE (from Beginning section)
(defvar current-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Let the coding begin! Be patient, %s!" current-user)

;; 4. PERFORMANCE ADJUSTMENTS (from Performance adjustments section)
(setq load-prefer-newer t)  ; Always load newest byte code
(setq read-process-output-max (* 1024 1024 2))  ; 2MB for LSP performance
(setq inhibit-compacting-font-caches t)  ; Don't compact fonts during GC

;; 5. NATIVE COMPILATION TWEAKS
(when (and (eq system-type 'darwin) (featurep 'native-compile))
  (let ((brew-prefix (or (getenv "HOMEBREW_PREFIX")
                         (when (file-exists-p "/opt/homebrew") "/opt/homebrew")
                         "/usr/local")))
    (when brew-prefix
      (setenv "LIBRARY_PATH" (concat brew-prefix "/lib/gcc/current:" brew-prefix "/lib"))
      (setenv "PATH" (concat brew-prefix "/bin:" (getenv "PATH")))
      (setq native-comp-async-report-warnings-errors 'silent
            native-comp-driver-options '("-Wl,-w")))))

;; 6. STRAIGHT.EL PERFORMANCE SETTINGS
(setq straight-check-for-modifications '(check-on-save find-when-checking))
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
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)


;; Async operations (from Async section)
(use-package async
  :demand t
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom
  (async-bytecomp-allowed-packages '(all)))

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
