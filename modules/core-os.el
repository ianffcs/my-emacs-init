;;; core-os.el --- OS-specific Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Operating system specific settings for macOS, Linux, and Windows.
;;
;; NOTE: This file has been cleaned up to remove duplications:
;; - pinentry → core-auth.el
;; - proced → core-session.el
;;
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. EXEC-PATH-FROM-SHELL (PATH synchronization)
;; ============================================================================

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-warn-duration-millis 1000)
  :config
  (dolist (var '("PATH" "MANPATH" "SSH_AUTH_SOCK" "GPG_AGENT_INFO"
                 "LANG" "LC_ALL" "LC_CTYPE"
                 "GOPATH" "GOROOT" "JAVA_HOME"
                 "NVM_DIR" "PYENV_ROOT" "RBENV_ROOT"
                 "MISE_SHELL" "RUSTUP_HOME" "CARGO_HOME"
                 "PNPM_HOME" "BUN_INSTALL"
                 "ANDROID_HOME" "ANDROID_SDK_ROOT"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; ============================================================================
;; 2. MACOS SPECIFIC
;; ============================================================================

(when (eq system-type 'darwin)
  ;; --- Modifier Keys ---
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-control-modifier 'control
        mac-right-option-modifier 'none  ; Allow special characters
        mac-function-modifier 'hyper)

  ;; --- Frame Behavior ---
  (setq ns-use-native-fullscreen t
        ns-pop-up-frames nil
        ns-use-proxy-icon nil)

  ;; --- Smooth Scrolling ---
  (setq mac-mouse-wheel-smooth-scroll t)

  ;; --- Trash ---
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash")

  ;; --- macOS Keybindings ---
  (global-set-key (kbd "s-a") #'mark-whole-buffer)
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-v") #'yank)
  (global-set-key (kbd "s-x") #'kill-region)
  (with-eval-after-load 'undo-fu
    (global-set-key (kbd "s-z") #'undo-fu-only-undo)
    (global-set-key (kbd "s-Z") #'undo-fu-only-redo))
  (global-set-key (kbd "s-s") #'save-buffer)
  (global-set-key (kbd "s-w") #'delete-window)
  (global-set-key (kbd "s-W") #'delete-frame)
  (global-set-key (kbd "s-n") #'make-frame-command)
  (global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
  (global-set-key (kbd "s-,") #'customize)
  (global-set-key (kbd "s-`") #'other-frame)
  (global-set-key (kbd "s-<return>") #'toggle-frame-fullscreen)

  ;; --- Reveal in Finder ---
  (defun ian/reveal-in-finder ()
    "Reveal the current file in Finder."
    (interactive)
    (if buffer-file-name
        (shell-command (format "open -R \"%s\"" buffer-file-name))
      (shell-command "open .")))

  (global-set-key (kbd "s-r") #'ian/reveal-in-finder)

  ;; --- Open with default app ---
  (defun ian/open-with-default-app ()
    "Open current file with default application."
    (interactive)
    (when buffer-file-name
      (shell-command (format "open \"%s\"" buffer-file-name))))

  (global-set-key (kbd "s-o") #'ian/open-with-default-app)

  ;; --- Dictionary lookup ---
  (defun ian/macos-dictionary ()
    "Look up word at point in macOS Dictionary."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (when word
        (shell-command (format "open dict://%s" word)))))

  (global-set-key (kbd "C-c d d") #'ian/macos-dictionary)

  ;; --- macOS Notifications ---
  (defun ian/macos-notify (title message)
    "Send macOS notification with TITLE and MESSAGE."
    (shell-command
     (format "osascript -e 'display notification \"%s\" with title \"%s\"'"
             message title)))

  ;; --- Homebrew paths ---
  (when (file-directory-p "/opt/homebrew")
    (add-to-list 'exec-path "/opt/homebrew/bin")
    (add-to-list 'exec-path "/opt/homebrew/sbin")
    (setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))))

;; ============================================================================
;; 3. LINUX SPECIFIC
;; ============================================================================

(when (eq system-type 'gnu/linux)
  ;; --- Trash ---
  (setq delete-by-moving-to-trash t)

  ;; --- Browser ---
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (or (executable-find "xdg-open")
                                       (executable-find "firefox")
                                       (executable-find "chromium")))

  ;; --- Open file manager ---
  (defun ian/open-file-manager ()
    "Open current directory in file manager."
    (interactive)
    (let ((dir (or (file-name-directory buffer-file-name)
                   default-directory)))
      (cond
       ((executable-find "nautilus")
        (start-process "file-manager" nil "nautilus" dir))
       ((executable-find "dolphin")
        (start-process "file-manager" nil "dolphin" dir))
       ((executable-find "thunar")
        (start-process "file-manager" nil "thunar" dir))
       (t
        (start-process "file-manager" nil "xdg-open" dir)))))

  (global-set-key (kbd "C-c f m") #'ian/open-file-manager)

  ;; --- Linux notifications ---
  (defun ian/linux-notify (title message)
    "Send Linux notification with TITLE and MESSAGE."
    (start-process "notify" nil "notify-send" title message))

  ;; --- Clipboard (X11/Wayland) ---
  (setq select-enable-clipboard t
        select-enable-primary t))

;; ============================================================================
;; 4. WINDOWS SPECIFIC
;; ============================================================================

(when (eq system-type 'windows-nt)
  ;; --- Paths ---
  (setq w32-pipe-read-delay 0
        w32-get-true-file-attributes nil)

  ;; --- Shell ---
  (setq explicit-shell-file-name "powershell.exe"
        shell-file-name "cmdproxy.exe")

  ;; --- Browser ---
  (setq browse-url-browser-function 'browse-url-default-windows-browser)

  ;; --- Fonts ---
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas-11")))

;; ============================================================================
;; 5. WSL (Windows Subsystem for Linux)
;; ============================================================================

(when (and (eq system-type 'gnu/linux)
           (string-match-p "microsoft" (shell-command-to-string "uname -r")))
  ;; --- Browser (use Windows browser) ---
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "wslview")

  ;; --- Clipboard integration ---
  (defun ian/wsl-copy (text)
    "Copy TEXT to Windows clipboard via clip.exe."
    (let ((process-connection-type nil))
      (let ((proc (start-process "clip" nil "clip.exe")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun ian/wsl-paste ()
    "Paste from Windows clipboard via powershell.exe."
    (shell-command-to-string
     "powershell.exe -command 'Get-Clipboard' 2>/dev/null | sed 's/\r$//'"))

  (setq interprogram-cut-function #'ian/wsl-copy)
  (setq interprogram-paste-function #'ian/wsl-paste))

;; ============================================================================
;; 6. COMMON SYSTEM UTILITIES
;; ============================================================================

;; --- System Encoding ---
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))

;; --- Time Zone ---
(setq system-time-locale "C")

;; ============================================================================
;; 7. KEYCHAIN (SSH/GPG Agent)
;; ============================================================================

(use-package keychain-environment
  :if (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  :config
  (keychain-refresh-environment))

;; ============================================================================
;; 8. HELPER FUNCTIONS
;; ============================================================================

(defun ian/system-info ()
  "Display system information."
  (interactive)
  (message "OS: %s | Emacs: %s | Host: %s | User: %s"
           system-type
           emacs-version
           (system-name)
           user-login-name))

(defun ian/copy-file-path ()
  "Copy the full path of the current file."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "Copied: %s" buffer-file-name)))

(defun ian/copy-file-name ()
  "Copy the name of the current file."
  (interactive)
  (when buffer-file-name
    (let ((name (file-name-nondirectory buffer-file-name)))
      (kill-new name)
      (message "Copied: %s" name))))

(defun ian/copy-directory-path ()
  "Copy the directory path of the current file."
  (interactive)
  (let ((dir (or (file-name-directory buffer-file-name)
                 default-directory)))
    (kill-new dir)
    (message "Copied: %s" dir)))

;; Keybindings
(global-set-key (kbd "C-c f p") #'ian/copy-file-path)
(global-set-key (kbd "C-c f n") #'ian/copy-file-name)
(global-set-key (kbd "C-c f d") #'ian/copy-directory-path)
(global-set-key (kbd "C-c f i") #'ian/system-info)

(provide 'core-os)
;;; core-os.el ends here
