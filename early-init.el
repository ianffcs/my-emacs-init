;;; early-init.el --- Early Initialization -*- lexical-binding: t; -*-
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq native-comp-async-report-warnings-errors nil)
(add-to-list 'warning-suppress-types '(native-compiler))
;; (setq native-comp-jit-compilation nil) ; Uncomment if you want to disable JIT entirely (rare)

;; GUI Emacs can inherit stale compiler overrides from launchd.  Native
;; packages such as vterm should let CMake select the system compiler when an
;; override no longer exists.
(dolist (variable '("CC" "CXX"))
  (let ((compiler (getenv variable)))
    (when (and compiler
               (file-name-absolute-p compiler)
               (not (file-executable-p compiler)))
      (setenv variable nil))))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

(setq initial-major-mode 'fundamental-mode)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Emacs tries to resize the frame when fonts change. This is slow and
;; looks glitchy during startup. We stop it.
(setq frame-inhibit-implied-resize t)

(when (and (eq system-type 'darwin) (featurep 'native-compile))
  (setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/current:/opt/homebrew/lib")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (setq native-comp-driver-options '("-Wl,-w")))

;; Seed exec-path from common locations so exec-path-from-shell can defer
;; to after-init without leaving the early frames PATH-less.
(when (eq system-type 'darwin)
  (dolist (p '("/opt/homebrew/bin" "/opt/homebrew/sbin"
               "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"))
    (unless (member p exec-path)
      (push p exec-path)))
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; Redirect native-compiled .eln files out of ~/.emacs.d root
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; gcmh handles GC tuning from here
            (setq file-name-handler-alist default-file-name-handler-alist)
            (makunbound 'default-file-name-handler-alist)
            (message "Emacs loaded in %.2fs with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'early-init)
;;; early-init.el ends here
