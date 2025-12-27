;;; tool-ai.el --- AI tools and assistants -*- lexical-binding: t; -*-

;;; Commentary:
;; AI assistants, LLM integration, speech input/output.
;; Migrated from README.org literate config.

;;; Code:

(require 'url)
(require 'json)
(require 'auth-source)

;; ============================================================================
;; 1. HELPER FUNCTIONS
;; ============================================================================

(defun ian/get-key (host)
  "Retrieve the password for HOST from auth-source."
  (let ((secret (plist-get (car (auth-source-search
                                 :host host
                                 :user "apikey"
                                 :require '(:secret)))
                           :secret)))
    (if secret
        (funcall secret)
      (error "No API key found for %s in .authinfo" host))))

(defun ian/authinfo-secret (host &optional user)
  "Return secret for HOST from auth-source. Default USER is \"apikey\"."
  (let* ((user (or user "apikey"))
         (entry (car (auth-source-search
                      :host host
                      :user user
                      :require '(:secret))))
         (secret (plist-get entry :secret)))
    (when secret
      (funcall secret))))

(defun ian/get-ollama-models (host-ip)
  "Fetch Ollama models using curl to bypass JSON parsing issues."
  (let* ((url (format "http://%s/api/tags" host-ip))
         (cmd (format "curl --noproxy '*' -s '%s'" url))
         (response (shell-command-to-string cmd))
         (models '()))
    (if (string-empty-p response)
        '("mistral:latest")
      (with-temp-buffer
        (insert response)
        (goto-char (point-min))
        (while (re-search-forward "\"name\":\"\\([^\"]+\\)\"" nil t)
          (push (match-string 1) models)))
      (if models
          (nreverse models)
        '("mistral:latest")))))

;; ============================================================================
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
;; 3. GPTEL (Main Chat Client)
;; ============================================================================

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :bind (("C-c g g" . gptel-send)
         ("C-c g G" . gptel)
         ("C-c g m" . gptel-menu)
         ("C-c g a" . gptel-add)
         ("C-c g f" . gptel-add-file)
         ("C-c g r" . gptel-rewrite))
  :config
  (setq gptel-default-mode 'org-mode)

  ;; -- OpenAI Backend --
  (setq gptel-openai-backend
        (gptel-make-openai "OpenAI"
          :key (ian/get-key "api.openai.com")
          :stream t
          :models '(gpt-4o gpt-4o-mini gpt-4-turbo gpt-3.5-turbo)))

  ;; -- Anthropic Backend --
  (setq gptel-anthropic-backend
        (gptel-make-anthropic "Anthropic"
          :key (ian/get-key "api.anthropic.com")
          :stream t
          :models '(claude-sonnet-4-20250514
                    claude-3-5-sonnet-20241022
                    claude-3-opus-20240229
                    claude-3-haiku-20240307)))

  ;; -- Gemini Backend --
  (setq gptel-gemini-backend
        (gptel-make-gemini "Gemini"
          :key (ian/get-key "generativelanguage.googleapis.com")
          :stream t))

  ;; -- Ollama Backend (Local) --
  (setq gptel-ollama-backend
        (gptel-make-ollama "Ollama"
          :host "192.168.178.161:11434"
          :stream t
          :models (ian/get-ollama-models "192.168.178.161:11434")))

  ;; -- Set Default Backend --
  (setq gptel-backend gptel-ollama-backend)
  (setq gptel-model 'qwen3-next-80b-fixed:latest)

  ;; Custom directives
  (setq gptel-directives
        '((default . "You are a helpful AI assistant.")
          (programming . "You are an expert programmer. Write clean, idiomatic code with clear comments.")
          (writing . "You are a writing assistant. Help improve clarity, grammar, and style.")
          (explain . "You are a patient teacher. Explain concepts clearly with examples.")
          (emacs . "You are an Emacs expert. Provide elisp solutions and configuration advice."))))

;; ============================================================================
;; 4. RAGMACS (Context Tools for Emacs)
;; ============================================================================

(use-package ragmacs
  :straight (:host github :repo "positron-solutions/ragmacs")
  :after gptel
  :config
  (add-to-list 'gptel-directives
               '(rag . "You are a helpful assistant with access to Emacs documentation."))
  (setq gptel-tools
        (list 'ragmacs-manuals
              'ragmacs-symbol-manual-node
              'ragmacs-manual-node-contents
              'ragmacs-function-source
              'ragmacs-variable-source)))

;; ============================================================================
;; 5. ELLAMA (Alternative LLM Assistant)
;; ============================================================================

(use-package ellama
  :commands (ellama-chat ellama-code-review ellama-summarize)
  :init
  (require 'llm-ollama)
  :config
  (setq ellama-provider
        (make-llm-ollama
         :chat-model "qwen3-next-80b-fixed:latest"
         :embedding-model "nomic-embed-text"
         :host "192.168.178.161"
         :port 11434))

  ;; Naming scheme for ellama sessions
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm))

;; ============================================================================
;; 6. ORG-AI (AI in Org-mode)
;; ============================================================================

(use-package org-ai
  :after org
  :commands (org-ai-mode org-ai-global-mode)
  :hook (org-mode . org-ai-mode)
  :bind (:map org-mode-map
              ("C-c M-a" . org-ai-complete)
              ("C-c M-r" . org-ai-on-region)
              ("C-c M-p" . org-ai-prompt)
              ("C-c M-s" . org-ai-summarize)
              ("C-c M-x" . org-ai-refactor-code)
              ("C-c M-!" . org-ai-open-request-buffer)
              ("C-c M-$" . org-ai-open-account-usage-url))
  :custom
  ;; --- API Configuration ---
  (org-ai-openai-api-token (ian/get-key "api.openai.com"))
  (org-ai-default-chat-model "gpt-4o")
  (org-ai-default-max-tokens 4096)
  (org-ai-default-chat-system-prompt
   "You are a helpful assistant working inside Emacs org-mode. Be concise and use org-mode formatting when appropriate.")

  ;; --- Behavior Settings ---
  (org-ai-auto-fill nil)
  (org-ai-talk-spoken-input t)
  (org-ai-image-directory (expand-file-name "org-ai-images" org-directory))

  ;; --- Model Selection ---
  (org-ai-default-completion-model "gpt-4o-mini")
  (org-ai-default-image-model "dall-e-3")
  (org-ai-image-default-size "1024x1024")
  (org-ai-image-default-count 1)
  (org-ai-image-default-style "vivid")

  :config
  ;; Enable global mode for AI blocks
  (org-ai-global-mode 1)

  ;; Install yasnippets for org-ai
  (org-ai-install-yasnippets)

  ;; --- Custom System Prompts ---
  (setq org-ai-chat-system-prompts
        '(("default" . "You are a helpful assistant working inside Emacs org-mode.")
          ("programmer" . "You are an expert programmer. Provide clean, well-documented code.")
          ("writer" . "You are a professional writer. Help improve clarity and style.")
          ("teacher" . "You are a patient teacher. Explain concepts step by step.")
          ("emacs-expert" . "You are an Emacs and Elisp expert. Provide idiomatic solutions.")
          ("researcher" . "You are a research assistant. Provide accurate, cited information.")
          ("translator" . "You are a professional translator. Translate accurately while preserving tone.")))

  ;; --- Helper Functions ---
  (defun ian/org-ai-complete-block ()
    "Insert an org-ai block and start completion."
    (interactive)
    (insert "#+begin_ai\n\n#+end_ai")
    (forward-line -1)
    (org-ai-complete))

  (defun ian/org-ai-chat-block ()
    "Insert an org-ai chat block."
    (interactive)
    (insert "#+begin_ai :chat t\n[ME]: \n#+end_ai")
    (search-backward "[ME]: ")
    (goto-char (match-end 0)))

  (defun ian/org-ai-code-block (lang)
    "Insert an org-ai block for code generation in LANG."
    (interactive "sLanguage: ")
    (insert (format "#+begin_ai :chat t\n[SYS]: You are an expert %s programmer. Write clean, well-documented code.\n\n[ME]: \n#+end_ai" lang))
    (search-backward "[ME]: ")
    (goto-char (match-end 0)))

  (defun ian/org-ai-summarize-buffer ()
    "Summarize the current buffer using org-ai."
    (interactive)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer (get-buffer-create "*org-ai-summary*")
        (erase-buffer)
        (org-mode)
        (insert "#+begin_ai :chat t\n")
        (insert "[SYS]: You are a summarization expert. Provide clear, concise summaries.\n\n")
        (insert "[ME]: Please summarize the following text:\n\n")
        (insert content)
        (insert "\n#+end_ai")
        (goto-char (point-min))
        (org-ai-complete)
        (switch-to-buffer (current-buffer)))))

  (defun ian/org-ai-explain-code ()
    "Explain the selected code using org-ai."
    (interactive)
    (if (use-region-p)
        (let ((code (buffer-substring-no-properties (region-beginning) (region-end)))
              (mode (symbol-name major-mode)))
          (with-current-buffer (get-buffer-create "*org-ai-explain*")
            (erase-buffer)
            (org-mode)
            (insert "#+begin_ai :chat t\n")
            (insert "[SYS]: You are a code explanation expert.\n\n")
            (insert (format "[ME]: Explain this %s code:\n\n```%s\n%s\n```\n" mode mode code))
            (insert "#+end_ai")
            (goto-char (point-min))
            (org-ai-complete)
            (switch-to-buffer (current-buffer))))
      (message "No region selected")))

  (defun ian/org-ai-improve-text ()
    "Improve the selected text using org-ai."
    (interactive)
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (with-current-buffer (get-buffer-create "*org-ai-improve*")
            (erase-buffer)
            (org-mode)
            (insert "#+begin_ai :chat t\n")
            (insert "[SYS]: You are a professional editor. Improve clarity, grammar, and style while preserving meaning.\n\n")
            (insert "[ME]: Please improve the following text:\n\n")
            (insert text)
            (insert "\n#+end_ai")
            (goto-char (point-min))
            (org-ai-complete)
            (switch-to-buffer (current-buffer))))
      (message "No region selected")))

  (defun ian/org-ai-translate (target-lang)
    "Translate the selected text to TARGET-LANG using org-ai."
    (interactive "sTranslate to language: ")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (with-current-buffer (get-buffer-create "*org-ai-translate*")
            (erase-buffer)
            (org-mode)
            (insert "#+begin_ai :chat t\n")
            (insert "[SYS]: You are a professional translator. Translate accurately while preserving tone and meaning.\n\n")
            (insert (format "[ME]: Translate the following text to %s:\n\n" target-lang))
            (insert text)
            (insert "\n#+end_ai")
            (goto-char (point-min))
            (org-ai-complete)
            (switch-to-buffer (current-buffer))))
      (message "No region selected")))

  ;; Additional keybindings
  (define-key org-mode-map (kbd "C-c M-b") #'ian/org-ai-complete-block)
  (define-key org-mode-map (kbd "C-c M-c") #'ian/org-ai-chat-block)
  (define-key org-mode-map (kbd "C-c M-C") #'ian/org-ai-code-block)
  (define-key org-mode-map (kbd "C-c M-S") #'ian/org-ai-summarize-buffer)
  (define-key org-mode-map (kbd "C-c M-e") #'ian/org-ai-explain-code)
  (define-key org-mode-map (kbd "C-c M-i") #'ian/org-ai-improve-text)
  (define-key org-mode-map (kbd "C-c M-t") #'ian/org-ai-translate))

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
  ;; macOS-specific audio device setup
  (when (eq system-type 'darwin)
    ;; Select default microphone
    (when (fboundp 'ian/select-default-audio-device)
      (ian/select-default-audio-device "MacBook Pro Microphone")))

  ;; List available macOS voices
  (defun ian/list-macos-voices ()
    "List available macOS voices for text-to-speech."
    (interactive)
    (shell-command "say -v '?'" "*macOS Voices*"))

  ;; Change voice interactively
  (defun ian/org-ai-set-voice ()
    "Interactively set the org-ai-talk voice."
    (interactive)
    (let* ((voices-output (shell-command-to-string "say -v '?' | cut -d' ' -f1"))
           (voices (split-string voices-output "\n" t))
           (voice (completing-read "Select voice: " voices nil t)))
      (setq org-ai-talk-say-voice voice)
      (message "Voice set to: %s" voice)))

  ;; Test voice
  (defun ian/org-ai-test-voice ()
    "Test the current org-ai-talk voice."
    (interactive)
    (let ((test-text "Hello, I am your AI assistant. How can I help you today?"))
      (shell-command (format "say -v '%s' -r %d '%s'"
                             org-ai-talk-say-voice
                             org-ai-talk-say-words-per-minute
                             test-text)))))

;; ============================================================================
;; 8. WHISPER (Speech-to-Text)
;; ============================================================================

(use-package whisper
  :straight (:type git :host github :repo "natrys/whisper.el")
  :commands whisper-run
  :bind ("C-c g w" . whisper-run)
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
  :bind ("C-c g s" . greader-mode)
  :custom
  (greader-espeak-rate 200)
  :config
  ;; Use macOS 'say' command if available
  (when (eq system-type 'darwin)
    (setq greader-tts-engine 'greader-say)))

;; ============================================================================
;; 10. CHATGPT-SHELL (Alternative Chat Interface)
;; ============================================================================

(use-package chatgpt-shell
  :commands chatgpt-shell
  :bind ("C-c g c" . chatgpt-shell)
  :custom
  (chatgpt-shell-openai-key (ian/get-key "api.openai.com"))
  (chatgpt-shell-model-version "gpt-4o-mini")
  (chatgpt-shell-system-prompt "You are a helpful assistant.")
  (chatgpt-shell-streaming t)
  (chatgpt-shell-highlight-blocks t)
  (chatgpt-shell-insert-dividers t))

(use-package dall-e-shell
  :commands dall-e-shell
  :custom
  (dall-e-shell-openai-key (ian/get-key "api.openai.com"))
  (dall-e-shell-image-size "1024x1024")
  (dall-e-shell-model-version "dall-e-3"))

;; ============================================================================
;; 11. COPILOT (GitHub Copilot - Optional)
;; ============================================================================

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :disabled  ; Enable if you have Copilot subscription
  :hook (prog-mode . copilot-mode))

;; ============================================================================
;; 12. MINUET (AI Completion in Buffer)
;; ============================================================================

(use-package minuet
  :straight (:type git :host github :repo "milanglacier/minuet-ai.el")
  :commands minuet-complete-with-minibuffer
  :bind ("M-RET" . minuet-complete-with-minibuffer)
  :custom
  (minuet-provider 'openai)
  :config
  (minuet-set-optional-backend-config
   'openai
   :model "gpt-4o-mini"
   :api-key (lambda () (ian/get-key "api.openai.com")))

  ;; Styling
  (set-face-attribute 'minuet-suggestion-face nil
                      :foreground "grey50"
                      :slant 'italic))

;; ============================================================================
;; 13. MCP (Model Context Protocol)
;; ============================================================================

(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el" :nonrecursive t)
  :after gptel
  :hook (after-init . mcp-hub-start-all-server)
  :config
  (require 'mcp-hub)
  (require 'subr-x)

  (setq mcp-hub-servers
        `(;; Filesystem access
          ("filesystem" . (:command "npx"
                           :args ("-y" "@modelcontextprotocol/server-filesystem"
                                  ,(if (boundp 'mcp-filesystem-server-project-root)
                                       (string-join mcp-filesystem-server-project-root " ")
                                     "/tmp"))))

          ;; DuckDuckGo search
          ("duckduckgo" . (:command ,(or (executable-find "uvx") "uvx")
                           :args ("duckduckgo-mcp-server")))

          ;; URL fetching
          ("fetch" . (:command ,(or (executable-find "uvx") "uvx")
                      :args ("mcp-server-fetch")))

          ;; Shell commands (restricted)
          ("mcp-shell-server" . (:command ,(or (executable-find "uvx") "uvx")
                                 :args ("mcp-shell-server")
                                 :env (:ALLOW_COMMANDS
                                       "bc,cat,chmod,curl,date,echo,find,git,grep,head,jq,ls,pwd,rg,sed,tail,wc")))

          ;; Clojure REPL (when CIDER is active)
          ,@(when (and (fboundp 'cider-current-repl)
                       (ignore-errors (cider-current-repl)))
              `(("clojure" . (:command "clojure"
                              :args ("-X:mcp"
                                     "--port"
                                     ,(number-to-string
                                       (cider-current-repl-port))))))))))

;; ============================================================================
;; 14. AIDER (AI Pair Programmer)
;; ============================================================================

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :commands (aider-transient-menu aider-run-aider)
  :bind ("C-c g p" . aider-transient-menu)
  :custom
  (aider-args '("--model" "gpt-4o-mini")))

;; ============================================================================
;; 15. AI TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/ai-menu ()
    "AI tools menu"
    ["Chat"
     ("g" "GPTel send" gptel-send)
     ("G" "GPTel buffer" gptel)
     ("c" "ChatGPT shell" chatgpt-shell)
     ("e" "Ellama chat" ellama-chat)]
    ["Org-AI"
     ("a" "Complete" org-ai-complete)
     ("r" "On region" org-ai-on-region)
     ("b" "Insert block" ian/org-ai-complete-block)
     ("C" "Chat block" ian/org-ai-chat-block)]
    ["Text"
     ("s" "Summarize" ian/org-ai-summarize-buffer)
     ("i" "Improve text" ian/org-ai-improve-text)
     ("t" "Translate" ian/org-ai-translate)
     ("x" "Explain code" ian/org-ai-explain-code)]
    ["Speech"
     ("w" "Whisper" whisper-run)
     ("W" "Whisper large" ian/whisper-run-large)
     ("T" "Talk toggle" org-ai-talk-toggle)
     ("R" "Read region" org-ai-talk-read-region)]
    ["Tools"
     ("m" "GPTel menu" gptel-menu)
     ("p" "Aider" aider-transient-menu)
     ("d" "DALL-E" dall-e-shell)]
    ["Settings"
     ("v" "Set voice" ian/org-ai-set-voice)
     ("V" "Test voice" ian/org-ai-test-voice)
     ("A" "Select audio" ian/select-default-audio-device)])

  (global-set-key (kbd "C-c g") #'ian/ai-menu))

;; ============================================================================
;; 16. KEYBINDING SUMMARY
;; ============================================================================

;; C-c g prefix for all AI commands:
;; C-c g     - AI transient menu
;; C-c g g   - gptel-send (send region/buffer to LLM)
;; C-c g G   - gptel (open chat buffer)
;; C-c g m   - gptel-menu (settings)
;; C-c g a   - gptel-add (add context)
;; C-c g f   - gptel-add-file (add file to context)
;; C-c g r   - gptel-rewrite (rewrite selection)
;; C-c g w   - whisper-run (speech-to-text)
;; C-c g s   - greader-mode (text-to-speech)
;; C-c g c   - chatgpt-shell
;; C-c g p   - aider menu
;; M-RET     - minuet completion
;;
;; C-c M- prefix for org-ai in org-mode:
;; C-c M-a   - org-ai-complete
;; C-c M-r   - org-ai-on-region
;; C-c M-p   - org-ai-prompt
;; C-c M-s   - org-ai-summarize
;; C-c M-x   - org-ai-refactor-code
;; C-c M-b   - insert org-ai block
;; C-c M-c   - insert chat block
;; C-c M-C   - insert code block (with lang)
;; C-c M-S   - summarize buffer
;; C-c M-e   - explain code
;; C-c M-i   - improve text
;; C-c M-t   - translate text
;; C-c M-T   - talk toggle
;; C-c M-R   - read region aloud

(provide 'tool-ai)
;;; tool-ai.el ends here
