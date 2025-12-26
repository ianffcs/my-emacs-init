;;; tool-media.el --- Media Player (EMMS with MPD) -*- lexical-binding: t; -*-

;;; Commentary:
;; Music player with EMMS and MPD integration.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. EMMS (Emacs Multimedia System)
;; ============================================================================

(use-package emms
  :commands (emms emms-smart-browse emms-pause emms-stop)
  :bind (;;("s-m p" . emms)
         ;;("s-m b" . emms-smart-browse)
         ;;("s-m r" . emms-player-mpd-update-all-reset-cache)
         ("<XF86AudioPrev>" . emms-previous)
         ("<XF86AudioNext>" . emms-next)
         ("<XF86AudioPlay>" . emms-pause)
         ("<XF86AudioStop>" . emms-stop))
  :custom
  (emms-seek-seconds 5)
  (emms-player-list '(emms-player-mpd))
  (emms-info-functions '(emms-info-mpd))
  (emms-player-mpd-server-name "localhost")
  (emms-player-mpd-server-port "6600")  ; Default MPD port
  (emms-source-file-default-directory "~/Music/")
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all))

;; ============================================================================
;; 2. MPC (MPD Client)
;; ============================================================================

(use-package mpc
  :straight (:type built-in)
  :commands mpc
  :custom
  (mpc-host "localhost:6600"))

;; ============================================================================
;; 3. MPD CONTROL FUNCTIONS
;; ============================================================================

(defun ian/mpd-start-music-daemon ()
  "Start MPD, connect to it and sync the metadata cache."
  (interactive)
  (shell-command "mpd")
  (ian/mpd-update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

(defun ian/mpd-kill-music-daemon ()
  "Stop playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))

(defun ian/mpd-update-database ()
  "Update the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))

;; Keybindings for MPD control
;;(global-set-key (kbd "s-m c") #'ian/mpd-start-music-daemon)
;;(global-set-key (kbd "s-m k") #'ian/mpd-kill-music-daemon)
;;(global-set-key (kbd "s-m u") #'ian/mpd-update-database)

;; ============================================================================
;; 4. VOLUME CONTROL
;; ============================================================================

(defun ian/volume-up ()
  "Increase system volume."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (shell-command "osascript -e 'set volume output volume ((output volume of (get volume settings)) + 5)'"))
   ((eq system-type 'gnu/linux)
    (shell-command "amixer set Master 2%+")))
  (message "Volume Up"))

(defun ian/volume-down ()
  "Decrease system volume."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (shell-command "osascript -e 'set volume output volume ((output volume of (get volume settings)) - 5)'"))
   ((eq system-type 'gnu/linux)
    (shell-command "amixer set Master 2%-")))
  (message "Volume Down"))

(defun ian/volume-toggle-mute ()
  "Toggle mute."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (shell-command "osascript -e 'set volume output muted not (output muted of (get volume settings))'"))
   ((eq system-type 'gnu/linux)
    (shell-command "amixer set Master toggle")))
  (message "Volume Toggled"))

;; Volume keybindings
(global-set-key (kbd "<XF86AudioRaiseVolume>") #'ian/volume-up)
(global-set-key (kbd "<XF86AudioLowerVolume>") #'ian/volume-down)
(global-set-key (kbd "<XF86AudioMute>") #'ian/volume-toggle-mute)

;; ============================================================================
;; 5. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/media-menu ()
                           "Media control commands"
                           ["EMMS"
                            ("p" "Player" emms)
                            ("b" "Browse" emms-smart-browse)
                            ("SPC" "Pause" emms-pause)
                            ("s" "Stop" emms-stop)]
                           ["Navigation"
                            ("n" "Next" emms-next)
                            ("P" "Previous" emms-previous)
                            ("f" "Forward" emms-seek-forward)
                            ("F" "Backward" emms-seek-backward)]
                           ["MPD"
                            ("c" "Start MPD" ian/mpd-start-music-daemon)
                            ("k" "Kill MPD" ian/mpd-kill-music-daemon)
                            ("u" "Update DB" ian/mpd-update-database)]
                           ["Volume"
                            ("+" "Up" ian/volume-up)
                            ("-" "Down" ian/volume-down)
                            ("m" "Mute" ian/volume-toggle-mute)])

  (global-set-key (kbd "s-m m") #'ian/media-menu))

(provide 'tool-media)
;;; tool-media.el ends here
