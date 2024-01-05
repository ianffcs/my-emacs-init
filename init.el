;;; init.el --- My configs
;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Do what you want, but learn your tool very very well.  This is my
;; attempt to keep my emacs-fu sharpe.

;;; Code:
(when (string-equal system-type "darwin")
  (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/11:/opt/homebrew/opt/libgccjit/lib/gcc/11:/opt/homebrew/opt/gcc/lib/gcc/11/gcc/aarch64-apple-darwin21/11"))
(defvar native-comp-deferred-compilation-deny-list nil)
(defconst emacs-start-time (current-time))

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

(setq vc-follow-symlinks t)
(straight-use-package '(org :type built-in))
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
