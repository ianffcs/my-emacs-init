;;; tool-chat.el --- AI chat and completion tools -*- lexical-binding: t; -*-

;;; Commentary:
;; AI chat and completion tools.

;;; Code:

(declare-function ian/ai-key "core-auth")
(defvar ian/ai-providers)
(declare-function ian/get-host "core-auth")
(declare-function ian/ai-key-callback "core-auth")
(declare-function ian/ollama-host "core-auth")
(declare-function ian/ollama-host-and-port "core-auth")
(declare-function ian/ai-available-provider "core-auth")
(require 'cl-lib)
(require 'subr-x)

;; 1. HELPER FUNCTIONS
;; ============================================================================

(defconst ian/ollama-chat-model "qwen3-next-80b-fixed:latest"
  "Default Ollama chat model for ellama and Minuet.")

(defconst ian/neotek-inference-models
  '(deepseek-v4-flash-vllm
    gpt-oss-120b
    gpt-oss-20b
    multilingual-e5-small
    nemotron-3-super-120b-a12b-nvfp4
    qwen3-14b-fp4
    qwen3-235b-a22b-fp4
    qwen3-8b-fp4
    qwen3-coder-30b-a3b-instruct
    qwen3-coder-30b-a3b-instruct-b
    qwen3-coder-next
    qwen3.5-122b-a10b-gptq-int4
    qwen3.6-35b-a3b-nvfp4)
  "Models advertised by the NeoTek OpenAI-compatible inference endpoint.")

(defun ian/ai-key-status ()
  "Show whether OpenAI and Claude API keys are available."
  (interactive)
  (let ((openai-ok (ian/ai-key 'openai t))
        (claude-ok (ian/ai-key 'anthropic t)))
    (message "AI keys -> OpenAI: %s | Claude: %s"
             (if openai-ok "ok" "missing")
             (if claude-ok "ok" "missing"))))

(defun ian/get-ollama-models (&optional host)
  "Return models advertised by Ollama at HOST.
HOST defaults to `ian/ollama-host'.  Return a conservative fallback when the
server is unavailable, so configuring gptel never makes Emacs unusable."
  (let* ((host (or host (ian/ollama-host)))
         (url (format "http://%s/api/tags" host))
         (response (with-temp-buffer
                     (when (zerop (call-process "curl" nil t nil
                                                "--noproxy" "*"
                                                "--connect-timeout" "3"
                                                "--max-time" "3"
                                                "-s" url))
                       (buffer-string))))
         (models '()))
    (if (not (stringp response))
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
;; 3. GPTEL (Main Chat Client)
;; ============================================================================

(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :bind ("C-c g" . gptel-menu)
  :config
  (setq gptel-default-mode 'org-mode)

  ;; -- Hosted Backends --
  ;; Register providers independently of whether credentials are available yet.
  (setq gptel-neotek-backend
        (gptel-make-openai "NeoTek"
          :host "infer.neotek.wg"
          :endpoint "/v1/chat/completions"
          :stream t
          :models ian/neotek-inference-models)
        gptel-openai-backend
        (gptel-make-openai "OpenAI"
          :key (ian/ai-key-callback 'openai)
          :stream t)
        gptel-anthropic-backend
        (gptel-make-anthropic "Anthropic"
          :key (ian/ai-key-callback 'anthropic)
          :stream t
          :models '(claude-sonnet-4-20250514
                    claude-3-5-sonnet-20241022
                    claude-3-opus-20240229
                    claude-3-haiku-20240307))
        gptel-gemini-backend
        (gptel-make-gemini "Gemini"
          :key (ian/ai-key-callback 'google)
          :stream t)
        gptel-ollama-backend
        (gptel-make-ollama "Ollama"
          :host (ian/ollama-host)
          :stream t
          :models (ian/get-ollama-models)))

  ;; Provider selection stays explicit in `gptel-menu'.
  (setq gptel-backend gptel-neotek-backend
        gptel-model (car ian/neotek-inference-models))

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
  :config
  (require 'llm-ollama)
  (setq ellama-provider
        (make-llm-ollama
         :chat-model ian/ollama-chat-model
         :embedding-model "nomic-embed-text"
         :host (car (ian/ollama-host-and-port))
         :port (cdr (ian/ollama-host-and-port))))

  ;; Naming scheme for ellama sessions
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm))

;; ============================================================================
;; 6. ORG-AI (AI in Org-mode)
;; ============================================================================

(defun ian/org-ai--get-token-with-shared-credentials (original &optional service)
  "Advice around `org-ai--openai-get-token' resolving tokens via shared credentials.
Preserve org-ai's native lookup for other services and for explicit token
settings."
  (let ((provider (or service org-ai-service)))
    (if (and (assq provider ian/ai-providers)
             (string-empty-p org-ai-openai-api-token))
        (or (ian/ai-key provider t) (funcall original service))
      (funcall original service))))

(use-package org-ai
  :after org
  :commands (org-ai-mode org-ai-global-mode)
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
  (org-ai-openai-api-token "")
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
  ;; org-ai has no token callback setting; adapt its request-time lookup.
  ;; Preserve its native lookup for other services and explicit token settings.
  (advice-add 'org-ai--openai-get-token :around
              #'ian/org-ai--get-token-with-shared-credentials
              '((name . shared-credentials)))
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
;; 10. CHATGPT-SHELL (Alternative Chat Interface)
;; ============================================================================

(use-package chatgpt-shell
  :commands chatgpt-shell
  :custom
  (chatgpt-shell-openai-key (ian/ai-key-callback 'openai))
  (chatgpt-shell-model-version "gpt-4o-mini")
  (chatgpt-shell-system-prompt "You are a helpful assistant.")
  (chatgpt-shell-streaming t)
  (chatgpt-shell-highlight-blocks t)
  (chatgpt-shell-insert-dividers t))

(use-package dall-e-shell
  :commands dall-e-shell
  :custom
  (dall-e-shell-openai-key (ian/ai-key-callback 'openai))
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
  (minuet-request-timeout 8)
  (minuet-n-completions 1)
  :config
  ;; Configure every provider without resolving credentials during package load.
  (plist-put minuet-openai-options :model "gpt-4.1-nano")
  (plist-put minuet-openai-options :api-key (ian/ai-key-callback 'openai))
  (minuet-set-optional-options minuet-openai-options :max_completion_tokens 128)
  (minuet-set-optional-options minuet-openai-options :reasoning_effort "none")
  (plist-put minuet-claude-options :model "claude-sonnet-4-20250514")
  (plist-put minuet-claude-options :api-key (ian/ai-key-callback 'anthropic))
  (minuet-set-optional-options minuet-claude-options :max_tokens 256)
  (plist-put minuet-openai-compatible-options :name "Ollama")
  (plist-put minuet-openai-compatible-options
             :end-point (format "http://%s/v1/chat/completions" (ian/ollama-host)))
  (plist-put minuet-openai-compatible-options :api-key "TERM")
  (plist-put minuet-openai-compatible-options :model ian/ollama-chat-model)
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)

  ;; These are the package's two request entry points, including auto-suggestion.
  ;; Named inline advice is replaced, rather than duplicated, on module reload.
  (dolist (command '(minuet-show-suggestion minuet-complete-with-minibuffer))
    (advice-add command :before
                (lambda (&rest _)
                  (setq minuet-provider (ian/ai-available-provider)))
                '((name . credential-provider-preference))))

  ;; Styling
  (set-face-attribute 'minuet-suggestion-face nil
                      :foreground "grey50"
                      :slant 'italic))

;; ============================================================================
;; 14. AIDER (AI Pair Programmer)
;; ============================================================================

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :commands (aider-transient-menu aider-run-aider)
  :custom
  (aider-args '("--model" "gpt-4o-mini")))

;; ============================================================================
;; 15. SHELL-MAKER (Shell process abstraction for agent-shell)
;; ============================================================================

(use-package shell-maker
  :straight (:host github :repo "xenodium/shell-maker"
             :pin "0.97.5"))

;; ============================================================================
;; 16. AGENT-SHELL (ACP coding agents: Claude, Codex, Kimi, Qwen)
;; ============================================================================

(use-package agent-shell
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code
             agent-shell-openai-start-codex
             agent-shell-kimi-start-agent
             agent-shell-qwen-start)
  :bind ("C-c M-g" . agent-shell)
  :config
  ;; Claude and Codex resolve API keys at session start, like the other
  ;; AI integrations.  Qwen and Kimi authenticate through their own CLIs
  ;; (/usr/bin/qwen, /usr/bin/kimi); inherit the session environment so
  ;; PATH, HOME, and the CLI login state are available to the agent.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (ian/ai-key-callback 'anthropic))
        agent-shell-openai-authentication
        (agent-shell-openai-make-authentication
         :api-key (ian/ai-key-callback 'openai))
        agent-shell-qwen-authentication
        (agent-shell-qwen-make-authentication :login t)
        agent-shell-qwen-environment
        (agent-shell-make-environment-variables :inherit-env t)
        agent-shell-kimi-environment
        (agent-shell-make-environment-variables :inherit-env t)))

;; KEYBINDINGS:
;; C-c g         - GPTel menu
;; C-c M-g       - agent-shell (Claude, Codex, Kimi, Qwen via ACP)
;; M-RET         - Minuet completion
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


(provide 'tool-chat)
;;; tool-chat.el ends here
