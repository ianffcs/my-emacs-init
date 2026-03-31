;;; lang-python.el --- Python -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Python development:
;; - Python mode with LSP (Eglot + Pyright/Pylsp)
;; - Virtual environment management
;; - Jupyter notebooks
;; - Testing and debugging
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. PYTHON MODE
;; ============================================================================

(use-package python
  :straight (:type built-in)
  :hook (python-mode . subword-mode)
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i")
  (python-indent-offset 4)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  ;; Use IPython if available
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-native-enable nil)))

(use-package python-ts-mode
  :straight (:type built-in)
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.pyw\\'" . python-ts-mode)
         ("\\.pyi\\'" . python-ts-mode))
  :hook (python-ts-mode . subword-mode))

;; ============================================================================
;; 2. VIRTUAL ENVIRONMENT MANAGEMENT
;; ============================================================================

;; Pyvenv - lightweight venv support
(use-package pyvenv
  :hook ((python-mode . pyvenv-mode)
         (python-ts-mode . pyvenv-mode))
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  :config
  (pyvenv-mode 1)

  ;; Auto-activate venv if .venv or venv directory exists
  (defun ian/pyvenv-autoload ()
    "Automatically activate virtualenv if present."
    (let ((venv-path (or (locate-dominating-file default-directory ".venv")
                         (locate-dominating-file default-directory "venv"))))
      (when venv-path
        (let ((venv-dir (expand-file-name
                         (if (file-exists-p (expand-file-name ".venv" venv-path))
                             ".venv"
                           "venv")
                         venv-path)))
          (when (file-directory-p venv-dir)
            (pyvenv-activate venv-dir))))))

  (add-hook 'python-mode-hook #'ian/pyvenv-autoload)
  (add-hook 'python-ts-mode-hook #'ian/pyvenv-autoload))

;; Virtualenvwrapper - for managing multiple virtualenvs
(use-package virtualenvwrapper
  :after python
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location (expand-file-name "~/.virtualenvs")))

;; Pipenv integration
(use-package pipenv
  :hook ((python-mode . pipenv-mode)
         (python-ts-mode . pipenv-mode))
  :custom
  (pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

;; Poetry integration
(use-package poetry
  :hook ((python-mode . poetry-tracking-mode)
         (python-ts-mode . poetry-tracking-mode)))

;; ============================================================================
;; 3. FORMATTING & LINTING
;; ============================================================================

(use-package python-black
  :after python
  :bind (:map python-mode-map
              ("C-c f b" . python-black-buffer)
              ("C-c f r" . python-black-region)
              :map python-ts-mode-map
              ("C-c f b" . python-black-buffer)
              ("C-c f r" . python-black-region)))

(use-package python-isort
  :after python
  :bind (:map python-mode-map
              ("C-c f i" . python-isort-buffer)
              :map python-ts-mode-map
              ("C-c f i" . python-isort-buffer)))

;; Flymake/Flycheck with Ruff (fast Python linter)
(use-package flymake-ruff
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))

;; ============================================================================
;; 4. TESTING
;; ============================================================================

(use-package python-pytest
  :after python
  :bind (:map python-mode-map
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-file)
              ("C-c t F" . python-pytest-file-dwim)
              ("C-c t m" . python-pytest-function)
              ("C-c t M" . python-pytest-function-dwim)
              ("C-c t r" . python-pytest-repeat)
              ("C-c t p" . python-pytest-popup)
              :map python-ts-mode-map
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-file)
              ("C-c t F" . python-pytest-file-dwim)
              ("C-c t m" . python-pytest-function)
              ("C-c t M" . python-pytest-function-dwim)
              ("C-c t r" . python-pytest-repeat)
              ("C-c t p" . python-pytest-popup))
  :custom
  (python-pytest-confirm t))

;; ============================================================================
;; 5. JUPYTER NOTEBOOKS
;; ============================================================================

;; EIN - Emacs IPython Notebook
(use-package ein
  :commands (ein:run ein:login ein:notebooklist-open)
  :bind (:map ein:notebook-mode-map
              ("C-c C-c" . ein:worksheet-execute-cell)
              ("C-c C-a" . ein:worksheet-execute-all-cells)
              ("C-c C-n" . ein:worksheet-goto-next-input)
              ("C-c C-p" . ein:worksheet-goto-prev-input)
              ("C-c C-k" . ein:worksheet-kill-cell)
              ("C-c C-y" . ein:worksheet-yank-cell)
              ("C-c C-o" . ein:worksheet-clear-output))
  :custom
  (ein:output-area-inlined-images t)
  (ein:slice-image t))

;; Code cells in regular Python files
(use-package code-cells
  :hook (python-mode . code-cells-mode-maybe)
  :bind (:map code-cells-mode-map
              ("M-p" . code-cells-backward-cell)
              ("M-n" . code-cells-forward-cell)
              ("C-c C-c" . code-cells-eval)))

;; ============================================================================
;; 6. DOCUMENTATION
;; ============================================================================

(use-package pydoc
  :after python
  :bind (:map python-mode-map
              ("C-c C-d" . pydoc-at-point)
              :map python-ts-mode-map
              ("C-c C-d" . pydoc-at-point)))

(use-package sphinx-doc
  :after python
  :hook ((python-mode . sphinx-doc-mode)
         (python-ts-mode . sphinx-doc-mode))
  :bind (:map python-mode-map
              ("C-c d d" . sphinx-doc)
              :map python-ts-mode-map
              ("C-c d d" . sphinx-doc)))

;; ============================================================================
;; 7. PIP & REQUIREMENTS
;; ============================================================================

(use-package pip-requirements
  :mode (("requirements.*\\.txt\\'" . pip-requirements-mode)
         ("requirements-.*\\.txt\\'" . pip-requirements-mode)
         ("requirements\\.in\\'" . pip-requirements-mode)))

;; ============================================================================
;; 8. DEBUGGING
;; ============================================================================

(use-package realgud
  :commands (realgud:pdb realgud:ipdb))

;; DAP mode debugging
(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               '(debugpy
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter")
                 :type "python"
                 :request "launch"
                 :program (lambda () (buffer-file-name))
                 :python (lambda () (pyvenv-virtualenv-name)))))

;; ============================================================================
;; 9. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-format ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-format ruff-isort))

  ;; Ruff formatters (faster than black+isort)
  (setf (alist-get 'ruff-format apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--fix" "--select" "I" "--stdin-filename" filepath "-")))

;; ============================================================================
;; 11. USEFUL FUNCTIONS
;; ============================================================================

(defun ian/python-insert-breakpoint ()
  "Insert a Python breakpoint() statement."
  (interactive)
  (insert "breakpoint()")
  (python-indent-line))

(defun ian/python-insert-ipdb ()
  "Insert an ipdb breakpoint."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()")
  (python-indent-line))

(defun ian/python-remove-breakpoints ()
  "Remove all breakpoint() and ipdb statements from buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^[[:space:]]*\\(breakpoint()\\|import i?pdb.*\\)$"
            nil t)
      (replace-match "")
      (delete-blank-lines))))

(defun ian/python-send-buffer-or-region ()
  "Send buffer or region to Python inferior process."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-buffer)))

(defun ian/python-venv-workon ()
  "Activate a virtualenv using pyvenv or virtualenvwrapper."
  (interactive)
  (if (fboundp 'pyvenv-workon)
      (call-interactively #'pyvenv-workon)
    (call-interactively #'venv-workon)))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c b b") #'ian/python-insert-breakpoint)
  (define-key python-mode-map (kbd "C-c b i") #'ian/python-insert-ipdb)
  (define-key python-mode-map (kbd "C-c b r") #'ian/python-remove-breakpoints)
  (define-key python-mode-map (kbd "C-c C-c") #'ian/python-send-buffer-or-region)
  (define-key python-mode-map (kbd "C-c v w") #'ian/python-venv-workon)
  (define-key python-mode-map (kbd "C-c v a") #'pyvenv-activate)
  (define-key python-mode-map (kbd "C-c v d") #'pyvenv-deactivate))

;; ============================================================================
;; 12. CYTHON
;; ============================================================================

(use-package cython-mode
  :mode "\\.pyx\\'")

;; ============================================================================
;; 13. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/python-menu ()
    "Python development commands"
    ["Virtual Env"
     ("v a" "Activate" pyvenv-activate)
     ("v d" "Deactivate" pyvenv-deactivate)
     ("v w" "Workon" ian/python-venv-workon)]
    ["Format"
     ("f b" "Black buffer" python-black-buffer)
     ("f r" "Black region" python-black-region)
     ("f i" "Isort" python-isort-buffer)]
    ["Test"
     ("t t" "Pytest dispatch" python-pytest-dispatch)
     ("t f" "Test file" python-pytest-file)
     ("t m" "Test function" python-pytest-function)
     ("t r" "Repeat" python-pytest-repeat)]
    ["Debug"
     ("b b" "Breakpoint" ian/python-insert-breakpoint)
     ("b i" "Ipdb" ian/python-insert-ipdb)
     ("b r" "Remove breakpoints" ian/python-remove-breakpoints)]
    ["REPL"
     ("r r" "Run Python" run-python)
     ("r s" "Send buffer/region" ian/python-send-buffer-or-region)])

  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c C-m") #'ian/python-menu))
  (with-eval-after-load 'python-ts-mode
    (define-key python-ts-mode-map (kbd "C-c C-m") #'ian/python-menu)))

(provide 'lang-python)
;;; lang-python.el ends here
