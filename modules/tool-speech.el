;;; tool-speech.el --- Speech input and output tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Speech input and output tools.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; 2. AUDIO DEVICE DETECTION (macOS - for Whisper/Speech)
;; ============================================================================

(when (eq system-type 'darwin)
  (defun ian/get-ffmpeg-device ()
    "Get the list of devices available to ffmpeg on macOS.
Returns two lists: (video-devices audio-devices).
Each list contains cons cells of (device-number . device-name)."
    (let ((lines (string-split
                  (shell-command-to-string
                   "ffmpeg -list_devices true -f avfoundation -i dummy 2>&1 || true")
                  "\n")))
      (cl-loop with at-video-devices = nil
               with at-audio-devices = nil
               with video-devices = nil
               with audio-devices = nil
               for line in lines
               when (string-match "AVFoundation video devices:" line)
               do (setq at-video-devices t at-audio-devices nil)
               when (string-match "AVFoundation audio devices:" line)
               do (setq at-audio-devices t at-video-devices nil)
               when (and at-video-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line))
                              (match-string 2 line))
                        video-devices)
               when (and at-audio-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line))
                              (match-string 2 line))
                        audio-devices)
               finally return (list (nreverse video-devices)
                                    (nreverse audio-devices)))))

  (defun ian/find-device-matching (string type)
    "Find device matching STRING of TYPE (:video or :audio)."
    (let* ((devices (ian/get-ffmpeg-device))
           (device-list (if (eq type :video) (car devices) (cadr devices))))
      (cl-loop for device in device-list
               when (string-match-p string (cdr device))
               return (car device))))

  (defcustom ian/default-audio-device nil
    "The default audio device to use for whisper and audio processes."
    :type 'integer
    :group 'ian)

  (defun ian/select-default-audio-device (&optional device-name)
    "Interactively select an audio device for whisper.
If DEVICE-NAME is provided, use it instead of prompting."
    (interactive)
    (let* ((audio-devices (cadr (ian/get-ffmpeg-device)))
           (names (mapcar #'cdr audio-devices))
           (name (or device-name (completing-read "Select audio device: " names nil t))))
      (setq ian/default-audio-device (ian/find-device-matching name :audio))
      (when (boundp 'whisper--ffmpeg-input-device)
        (setq whisper--ffmpeg-input-device
              (format ":%s" ian/default-audio-device)))
      (message "Audio device set to: %s (index %d)" name ian/default-audio-device))))

;; ============================================================================
;; 7. ORG-AI TALK (Speech Input/Output)
;; ============================================================================

(use-package org-ai-talk
  :straight nil  ; Part of org-ai
  :after org-ai
  :bind (:map org-mode-map
              ("C-c M-T" . org-ai-talk-toggle)
              ("C-c M-R" . org-ai-talk-read-region))
  :custom
  ;; --- Speech-to-Text (Whisper) ---
  (org-ai-talk-whisper-enable t)

  ;; --- Text-to-Speech ---
  ;; macOS speech settings
  (org-ai-talk-say-words-per-minute 210)
  (org-ai-talk-say-voice "Samantha")  ; or "Karen", "Daniel", "Moira", etc.

  :config
  ;; macOS-specific audio device setup — deferred to avoid blocking ffmpeg call on org-mode open
  (when (eq system-type 'darwin)
    (run-with-idle-timer 3 nil
      (lambda ()
        (when (fboundp 'ian/select-default-audio-device)
          (ian/select-default-audio-device "MacBook Pro Microphone")))))

  ;; List available macOS voices
  (defun ian/list-macos-voices ()
    "List available macOS voices for text-to-speech."
    (interactive)
    (shell-command "say -v '?'" "*macOS Voices*"))

  ;; Change voice interactively
  (defun ian/org-ai-set-voice ()
    "Interactively set the org-ai-talk voice."
    (interactive)
    (let* ((voices (mapcar (lambda (line)
                             (car (split-string line " " t)))
                           (process-lines "say" "-v" "?")))
           (voice (completing-read "Select voice: " voices nil t)))
      (setq org-ai-talk-say-voice voice)
      (message "Voice set to: %s" voice)))

  ;; Test voice
  (defun ian/org-ai-test-voice ()
    "Test the current org-ai-talk voice."
    (interactive)
    (let ((test-text "Hello, I am your AI assistant. How can I help you today?"))
      (start-process "org-ai-test-voice" nil
                     "say"
                     "-v" org-ai-talk-say-voice
                     "-r" (number-to-string org-ai-talk-say-words-per-minute)
                     test-text))))

;; ============================================================================
;; 8. WHISPER (Speech-to-Text)
;; ============================================================================

(use-package whisper
  :straight (:type git :host github :repo "natrys/whisper.el")
  :commands whisper-run
  :custom
  (whisper-model "base")
  (whisper-language "en")
  (whisper-translate nil)
  (whisper-install-directory (expand-file-name "whisper" user-emacs-directory))
  (whisper-return-cursor-to-start t)
  (whisper-insert-text-at-point t)
  :config
  ;; macOS audio device setup
  (when (eq system-type 'darwin)
    (when (and (boundp 'ian/default-audio-device) ian/default-audio-device)
      (setq whisper--ffmpeg-input-device
            (format ":%s" ian/default-audio-device))))

  ;; Whisper with different models
  (defun ian/whisper-run-large ()
    "Run whisper with the large model for better accuracy."
    (interactive)
    (let ((whisper-model "large"))
      (whisper-run)))

  (defun ian/whisper-run-translate ()
    "Run whisper with translation to English enabled."
    (interactive)
    (let ((whisper-translate t))
      (whisper-run))))

;; ============================================================================
;; 9. GREADER (Text-to-Speech Reader)
;; ============================================================================

(use-package greader
  :commands greader-mode
  :custom
  (greader-espeak-rate 200)
  :config
  ;; Use macOS 'say' command if available
  (when (eq system-type 'darwin)
    (setq greader-tts-engine 'greader-say)))

;; ============================================================================

(provide 'tool-speech)
;;; tool-speech.el ends here
