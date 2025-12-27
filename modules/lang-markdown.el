;;; lang-markdown.el --- Markdown & Writing Tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Markdown and prose writing:
;; - Markdown mode and GFM
;; - ReStructuredText and AsciiDoc
;; - Spell and grammar checking
;; - Focus/distraction-free writing
;; - Word count and statistics
;; - Typographic enhancements
;;
;; Split from lang-text.el for better organization.

;;; Code:

;; ============================================================================
;; 1. MARKDOWN
;; ============================================================================

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode))
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode)
         (markdown-mode . ian/markdown-setup))
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-export)
              ("C-c C-p" . markdown-preview)
              ("C-c C-o" . markdown-follow-thing-at-point)
              ("C-c '" . markdown-edit-code-block))
  :custom
  (markdown-command "pandoc")
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh" "elisp" "clojure" "python" "rust"))
  (markdown-header-scaling t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-hide-urls nil)
  (markdown-hr-display-char ?─)
  :config
  (defun ian/markdown-setup ()
    "Custom markdown mode setup."
    (setq-local fill-column 80)
    (setq-local truncate-lines nil)
    (setq-local word-wrap t))

  (defun ian/markdown-set-faces ()
    "Set markdown faces for better readability."
    (variable-pitch-mode 1)
    (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-inline-code-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch))

  (add-hook 'markdown-mode-hook #'ian/markdown-set-faces))

;; Live preview
(use-package markdown-preview-mode
  :after markdown-mode
  :commands markdown-preview-mode)

;; Table of contents
(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

;; Edit code blocks in separate buffer
(use-package edit-indirect
  :after markdown-mode)

;; ============================================================================
;; 2. GITHUB FLAVORED MARKDOWN
;; ============================================================================

(use-package grip-mode
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-g" . grip-mode))
  :custom
  (grip-preview-use-webkit nil))

;; ============================================================================
;; 3. RESTRUCTUREDTEXT
;; ============================================================================

(use-package rst
  :straight (:type built-in)
  :mode (("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode))
  :hook ((rst-mode . flyspell-mode)
         (rst-mode . visual-line-mode)))

;; Sphinx documentation
(use-package sphinx-mode
  :after rst
  :hook (rst-mode . sphinx-mode))

;; ============================================================================
;; 4. ASCIIDOC
;; ============================================================================

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)
         ("\\.asciidoc\\'" . adoc-mode))
  :hook ((adoc-mode . flyspell-mode)
         (adoc-mode . visual-line-mode)))

;; ============================================================================
;; 5. JINJA2 TEMPLATES
;; ============================================================================

(use-package jinja2-mode
  :mode (("\\.j2\\'" . jinja2-mode)
         ("\\.jinja2?\\'" . jinja2-mode)))

;; ============================================================================
;; 6. SPELL CHECKING
;; ============================================================================

(use-package flyspell
  :straight (:type built-in)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :config
  ;; Use aspell if available
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

  ;; Or use hunspell
  (when (and (not (executable-find "aspell"))
             (executable-find "hunspell"))
    (setq ispell-program-name "hunspell"
          ispell-local-dictionary "en_US")))

;; Spell correction with Consult
(use-package consult-flyspell
  :after (consult flyspell)
  :bind (:map flyspell-mode-map
              ("C-;" . consult-flyspell)))

;; Spell-fu - faster spell checking
(use-package spell-fu
  :hook (text-mode . spell-fu-mode)
  :custom
  (spell-fu-faces-exclude '(org-meta-line org-link org-code org-block)))

;; ============================================================================
;; 7. GRAMMAR CHECKING
;; ============================================================================

;; LanguageTool integration
(use-package langtool
  :commands (langtool-check langtool-correct-buffer)
  :custom
  (langtool-language-tool-jar (expand-file-name "~/.local/share/languagetool/languagetool-commandline.jar"))
  (langtool-default-language "en-US"))

;; Writegood mode - highlight weak writing
(use-package writegood-mode
  :hook ((markdown-mode . writegood-mode)
         (org-mode . writegood-mode)
         (latex-mode . writegood-mode))
  :bind ("C-c w" . writegood-mode))

;; ============================================================================
;; 8. FOCUS & DISTRACTION-FREE WRITING
;; ============================================================================

;; Olivetti - centered text
(use-package olivetti
  :hook ((text-mode . olivetti-mode)
         (org-mode . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :custom
  (olivetti-body-width 80)
  (olivetti-minimum-body-width 72))

;; Focus mode - dim surrounding text
(use-package focus
  :commands focus-mode
  :custom
  (focus-mode-to-thing '((prog-mode . defun)
                         (text-mode . paragraph))))

;; Darkroom - distraction-free writing
(use-package darkroom
  :commands darkroom-mode)

;; ============================================================================
;; 9. WORD COUNT & STATISTICS
;; ============================================================================

(use-package wc-mode
  :hook ((markdown-mode . wc-mode)
         (org-mode . wc-mode)
         (latex-mode . wc-mode))
  :custom
  (wc-modeline-format "[%tw words]"))

;; Count words in region/buffer
(defun ian/count-words-buffer ()
  "Count words in buffer and display."
  (interactive)
  (message "Words: %d | Characters: %d | Lines: %d"
           (count-words (point-min) (point-max))
           (- (point-max) (point-min))
           (count-lines (point-min) (point-max))))

(global-set-key (kbd "C-c C-w") #'ian/count-words-buffer)

;; ============================================================================
;; 10. TYPOGRAPHIC ENHANCEMENTS
;; ============================================================================

;; Smart quotes and dashes
(use-package typo
  :hook ((markdown-mode . typo-mode)
         (org-mode . typo-mode))
  :custom
  (typo-language "English"))

;; Unicode input
(use-package company-emoji
  :after company)

;; ============================================================================
;; 11. PANDOC (Universal Document Converter)
;; ============================================================================

(use-package pandoc-mode
  :hook ((markdown-mode . pandoc-mode)
         (rst-mode . pandoc-mode)
         (org-mode . pandoc-mode))
  :bind (:map pandoc-mode-map
              ("C-c p" . pandoc-main-hydra/body)))

;; ============================================================================
;; 12. NOVELS & CREATIVE WRITING
;; ============================================================================

(use-package fountain-mode
  :mode "\\.fountain\\'"
  :custom
  (fountain-export-command-pdf "afterwriting"))

;; ============================================================================
;; 13. LOG FILES
;; ============================================================================

(use-package logview
  :mode (("\\.log\\'" . logview-mode)
         ("log\\'" . logview-mode)))

(use-package syslog-mode
  :mode (("/var/log.*\\'" . syslog-mode)
         ("\\.syslog\\'" . syslog-mode)))

;; ============================================================================
;; 14. CSV & TSV
;; ============================================================================

(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . csv-mode))
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-separators '("," ";" "|" "\t")))

;; ============================================================================
;; 15. LEDGER (Plain Text Accounting)
;; ============================================================================

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :custom
  (ledger-clear-whole-transactions t))

;; ============================================================================
;; 16. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Markdown (prettier)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'gfm-mode apheleia-mode-alist) '(prettier)))

;; ============================================================================
;; 17. USEFUL WRITING FUNCTIONS
;; ============================================================================

(defun ian/writing-mode ()
  "Enable distraction-free writing environment."
  (interactive)
  (olivetti-mode 1)
  (display-line-numbers-mode -1)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1)
  (wc-mode 1))

(defun ian/toggle-prose-mode ()
  "Toggle between prose and code editing modes."
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode -1)
        (variable-pitch-mode -1)
        (display-line-numbers-mode 1))
    (progn
      (olivetti-mode 1)
      (variable-pitch-mode 1)
      (display-line-numbers-mode -1))))

(defun ian/insert-date ()
  "Insert current date in ISO format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun ian/insert-datetime ()
  "Insert current date and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun ian/titlecase-region (beg end)
  "Convert region from BEG to END to title case."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((word (thing-at-point 'word t)))
          (when word
            (delete-region (match-beginning 0) (match-end 0))
            (insert (capitalize word))))
        (forward-word)))))

(global-set-key (kbd "C-c t w") #'ian/writing-mode)
(global-set-key (kbd "C-c t p") #'ian/toggle-prose-mode)
(global-set-key (kbd "C-c t d") #'ian/insert-date)

(provide 'lang-markdown)
;;; lang-markdown.el ends here
