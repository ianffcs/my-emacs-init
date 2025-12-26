;;; early-init.el --- Early Initialization -*- lexical-binding: t; no-byte-compile: t -*-
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq native-comp-async-report-warnings-errors 'silent)
;; (setq native-comp-jit-compilation nil) ; Uncomment if you want to disable JIT entirely (rare)

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

(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)

            ;; Clean up the temporary variable
            (makunbound 'default-file-name-handler-alist)

            (message "🚀 Emacs loaded in %.2fs with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'early-init)
;;; early-init.el ends here
