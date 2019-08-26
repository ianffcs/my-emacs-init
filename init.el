;;;; Initialization

 (defun tangle-init ()
   "If the current buffer is 'init.org' the code-blocks are
   tangled, and the tangled file is compiled."
   (when (equal (buffer-file-name)
                 (expand-file-name (concat user-emacs-directory "init.org")))
     ;; Avoid running hooks when tangling.
     (let ((prog-mode-hook nil))
        (org-babel-tangle)
     ;   (byte-compile-file (concat user-emacs-directory "init.el"))
     )))

 (add-hook 'after-save-hook 'tangle-init)

;(eval-when-compile
; (setq use-package-expand-minimally byte-compile-current-file))
;; ;;-----------------
  ;; -----------------------------------------------------------
  ;; Adjust garbage collection thresholds during startup, and thereafter
  ;;----------------------------------------------------------------------------
  ;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
  ;;       (init-gc-cons-threshold (* 128 1024 1024)))
  ;;   (setq gc-cons-threshold init-gc-cons-threshold)
  ;;   (add-hook 'emacs-startup-hook
  ;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

  ;;;; Packaging
  (setq package-enable-at-startup nil)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ;;("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("tromey" . "http://tromey.com/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")))

  (package-initialize)

  ;;;; use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile (require 'use-package))

  (setq use-package-always-ensure t)

(setq custom-file (expand-file-name (concat user-emacs-directory "custom/custom.el")))
(load custom-file)

(defun paste-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'copy-to-osx)
(setq interprogram-paste-function 'paste-from-osx)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; Check the system
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

(defconst private-dir (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir))

(unless (file-exists-p private-dir)
  (make-directory private-dir :parents))

(unless (file-exists-p temp-dir)
  (make-directory temp-dir :parents))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
                                        ; default window width and height
                                        ; flashes the cursor's line when you scroll
  (use-package beacon
    :ensure t
    :config
    (beacon-mode 1)
                                        ; (setq beacon-color "#666600")
    )

                                        ; expand the marked region in semantic increments (negative prefix to reduce region)
  (use-package expand-region
    :ensure t
    :config
    (global-set-key (kbd "C-=") 'er/expand-region))

                                        ; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package origami
  :ensure t)

(global-origami-mode)

      ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
        logical line.  This is useful, e.g., for use with
        `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(use-package mode-icons
 :config (mode-icons-mode))

(global-prettify-symbols-mode)

(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t
        mode-line-format
        (list '(:eval (list (nyan-create)))))
  (nyan-mode t))

(use-package parrot
  :config
  (global-set-key (kbd "C-c p") 'parrot-rotate-prev-word-at-point)
  (global-set-key (kbd "C-c n") 'parrot-rotate-next-word-at-point)
  (parrot-set-parrot-type 'thumbsup)
  (parrot-mode)
  (add-hook 'before-save-hook 'parrot-start-animation))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package wakatime-mode
  :config (global-wakatime-mode))

(defalias 'yes-or-no-p 'y-or-n-p)
;; before save clears whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "C-c i") 'string-inflection-all-cycle)

(set-charset-priority 'unicode)
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system          'utf-8)

  ;; (defconst my-default-font "-*-fixed-medium-r-normal-*-15-*-*-*-*-*-*-*")
  ;; (defconst my-default-font "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*")
  ;; (defconst my-default-font "-b&h-lucidatypewriter-medium-r-normal-sans-14-*-*-*-*-*-iso8859-1")
  ;; (defconst my-default-font "FantasqueSansMono Nerd Font-10")
  ;; (defconst my-default-font "Monoid-9")
  ;; (defconst my-default-font "Fixed-10")
  ;;(defconst my-default-font "Dina-10")
  ;; (defconst my-default-font "Iosevka-9")
  ;; (defconst my-default-font "Terminus-10")
  ;; (defconst my-default-font "Fira Code-10")
  (defconst my-default-font "Hack-10")

   (defconst my-frame-alist
     `((font                 . ,my-default-font)
       (scroll-bar           . -1)
       (height               . 60)
       (width                . 95)
       (alpha                . 95)
       (vertical-scroll-bars . nil)))
   (setq default-frame-alist my-frame-alist)

   (use-package all-the-icons
     :ensure t)

   (use-package doom-themes
     :ensure t
     :init (setq doom-themes-enable-bold t doom-themes-enable-italic t)
     :config
     (doom-themes-org-config)
                                           ; (doom-themes-treemacs-config)
     (load-theme 'doom-one t))

   (setq inhibit-startup-screen        t
         inhibit-splash-screen         t
         line-number-mode              1
         column-number-mode            1
         show-paren-mode               1
         show-paren-delay              0
         blink-cursor-mode             nil
         transient-mark-mode           1
         scroll-bar-mode               -1
         browser-url-browse-function   'browse-url-firefox
         linum-format                  "%5d"
         tab-width                     4
         global-hl-line-mode           t
         indent-tabs-mode              nil
         truncate-partial-width-windows 1
         fill-column                   80
         truncate-lines                1
         save-interprogram-paste-before-kill t
         ;; Mouse
         transentient-mark-mode        t
         mouse-wheel-follow-mouse      t
         scroll-step                   1
         scroll-conservatively         101
         mouse-wheel-scroll-amount     '(1)
         mouse-wheel-progressive-speed nil)

  (menu-bar-mode -99)
  (tool-bar-mode -1)

     (defun custom-set-frame-size ()
       (add-to-list 'default-frame-alist '(height . 50))
       (add-to-list 'default-frame-alist '(width . 178)))
     (custom-set-frame-size)
     (add-hook 'before-make-frame-hook 'custom-set-frame-size)

     (use-package rainbow-delimiters
       :ensure t
       :config
       (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'common-lisp-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

   (add-hook 'prog-mode-hook 'linum-mode)

   (defun set-frame-alpha (value)
     "Set the transparency of the frame. 0 = transparent/100 = opaque"
     (interactive "Alpha value (0-100): ")
     (set-frame-parameter (selected-frame) 'alpha value))

   (set-frame-alpha 90)

  (use-package highlight-numbers
    :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package flycheck)

(use-package flycheck-joker)

(use-package flycheck-clj-kondo)

  (require 'semantic)

  (global-semanticdb-minor-mode        1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode     0)

  (semantic-mode 1)

  (use-package company
    :ensure t
    :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3)
    (global-company-mode t))

  (use-package company-irony
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony))

  (use-package irony
    :ensure t
    :config
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc))

  ;; (defun my/python-mode-hook ()
  ;;   (add-to-list 'company-backends 'company-jedi))

  ;; (add-hook 'python-mode-hook 'my/python-mode-hook)
  ;; (use-package company-jedi
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'python-mode-hook 'jedi:setup))

  ;; (defun my/python-mode-hook ()
  ;;   (add-to-list 'company-backends 'company-jedi))

  ;; (add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package slime
  :mode
  ("\\.lisp$" . slime-mode)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"
        slime-net-coding-system 'utf-8-unix
        slime1-contribs '(slime-fancy)))

   (use-package cider
     :ensure t
     :config
     (progn (add-hook 'clojure-mode-hook 'cider-mode)
            (add-hook 'clojure-mode-hook 'clj-refactor-mode)
            (add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
            (add-hook 'cider-repl-mode-hook 'subword-mode)
            (setq cider-annotate-completion-candidates t
                  cider-prompt-for-symbol nil)
            (eval-after-load 'cider
              #'emidje-enable-nrepl-middleware))
     (setq cider-repl-pop-to-buffer-on-connect 'display-only)
     (setq cider-repl-use-clojure-font-lock nil)
     (setq cider-repl-use-pretty-printing t)
     (setq cider-repl-wrap-history t)
     (setq cider-repl-pop-to-buffer-on-connect 'display-only)
     (setq cider-repl-result-prefix ";; => ")
     (setq cider-repl-display-in-current-window t)
     (setq cider-repl-wrap-history t)
     (setq cider-repl-use-pretty-printing 't)
     (setq cider-pprint-fn 'puget)
     (setq cider-print-options '(("print-color" "true")))
     (setq cider-repl-use-clojure-font-lock t)
     (add-hook 'cider-repl-mode-hook #'company-mode)
     (add-hook 'cider-mode-hook #'company-mode)
     (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
     (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
     (eval-after-load 'cider #'emidje-enable-nrepl-middleware)
     (setq cider-auto-select-error-buffer nil)
     (setq org-babel-clojure-backend 'cider)
     (setq-default emidje-load-facts-on-eval t))

   (use-package clj-refactor
     :ensure t
     :config (progn (setq cljr-suppress-middleware-warnings t)
                    (add-hook 'clojure-mode-hook (lambda ()
                           (clj-refactor-mode 1)
                           (yas-minor-mode 1)
                           (cljr-add-keybindings-with-prefix "C-c C-m")))))

   (use-package emidje
     :ensure t
     :config (eval-after-load 'cider #'emidje-setup))

   ;; (use-package highlight-parentheses :ensure t)
     ;; :config
     ;; (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
     ;; (add-hook 'clojurescript-mode-hook #'smartparens-strict-mode)
     ;; (add-hook 'clojure-mode-hook (lambda () (sp-local-pair '(clojure-mode) "'" "'" :actions nil))))

   (use-package clojure-mode
     :ensure t
     :config
      (require 'flycheck-joker)
      (require 'flycheck-clj-kondo)
      (setq clojure-align-forms-automatically t)
      (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
      (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
      (dolist (checkers '((clj-kondo-clj . clojure-joker)
      (clj-kondo-cljs . clojurescript-joker)
      (clj-kondo-cljc . clojure-joker)
      (clj-kondo-edn . edn-joker)))
      (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))


   (defun set-auto-complete-as-completion-at-point-function ()
     (setq completion-at-point-functions '(auto-complete)))
   (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
   (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
   (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
   (eval-after-load "cider"
     '(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc))

(use-package dockerfile-mode
  :mode ("\\Dockerfile$" . dockerfile-mode))

(use-package docker-compose-mode)

(defun format-elixir-buffer ()
  "Format elixir buffer."
  (add-hook 'before-save-hook 'elixir-format nil t))

(use-package elixir-mode
  :hook ((elixir-mode . format-elixir-buffer)
         (elixir-mode . flycheck-mix-setup))
  :mode (("\\.ex$" . elixir-mode)
         ("\\.exs$" . elixir-mode)))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode))

(use-package flycheck-mix)

(use-package nginx-mode)

(use-package python
  :mode ("\\.py" . python-mode)
  :config (setq python-shell-interpreter "python3"
                py-python-command "python3"))

(use-package elpy
  :hook ((python-mode . elpy-mode)
         (python-mode . elpy-enable))
  :custom
  (elpy-rpc-backend "jedi")
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("C-c i" . elpy-autopep8-fix-code)
              ("C-c C-d" . elpy-doc)))

(use-package pip-requirements
  :hook ((pip-requirements-mode . #'pip-requirements-auto-complete-setup)))

(use-package py-autopep8
  :hook ((python-mode . py-autopep8-enable-on-save)))

(use-package virtualenvwrapper
    :ensure t
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package ein)

(use-package hy-mode)

  ;; rust-mode
  ;; https://github.com/rust-lang/rust-mode
  (use-package rust-mode
    :bind ( :map rust-mode-map
           (("C-c C-t" . racer-describe)))
    :config
    (progn
      ;; add flycheck support for rust
      ;; https://github.com/flycheck/flycheck-rust
      (use-package flycheck-rust
       :after rust-mode
       :hook ((rust-mode . flycheck-rust-setup)))

      ;; cargo-mode for all the cargo related operations
      ;; https://github.com/kwrooijen/cargo.el
      (use-package cargo
         :hook ((rust-mode . cargo-minor-mode)))

      ;; racer-mode for getting IDE like features for rust-mode
      ;; https://github.com/racer-rust/emacs-racer
      (use-package racer
        :hook ((rust-mode . racer-mode)
               (racer-mode . eldoc-mode))
        :config
        (progn
          ;; set racer rust source path environment variable
          (setq racer-rust-src-path "/home/ianffcs/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
          (defun my-racer-mode-hook ()
            (set (make-local-variable 'company-backends)
                 '((company-capf company-files))))
           (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

          ;; enable company and eldoc minor modes in rust-mode
          (add-hook 'racer-mode-hook 'company-mode)
          (add-hook 'racer-mode-hook 'eldoc-mode)))

      (add-hook 'rust-mode-hook 'flycheck-mode)
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
      (add-hook 'rust-mode-hook 'racer-mode)
      (add-hook 'rust-mode-hook 'cargo-minor-mode)

      ;; format rust buffers on save using rustfmt
      (add-hook 'before-save-hook
                (lambda ()
                  (when (eq major-mode 'rust-mode)
                    (rust-format-buffer)))))

  (use-package haskell-mode
    :ensure t)

  (use-package intero
    :ensure t :config
    (progn
      (add-hook 'haskell-mode-hook 'intero-mode)))

  (setq flycheck-check-syntax-automatically '(save new-line))
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(use-package web-mode
    :ensure t
    :config
           (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
           (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
           (setq web-mode-engines-alist
                 '(("django"    . "\\.html\\'")))
           (setq web-mode-ac-sources-alist
           '(("css" . (ac-source-css-property))
           ("vue" . (ac-source-words-in-buffer ac-source-abbrev))
         ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
(setq web-mode-enable-auto-closing t))
(setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned

(use-package js2-mode
:ensure t
:ensure ac-js2
:init
(progn
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
))

(use-package js2-refactor
:ensure t
:config
(progn
(js2r-add-keybindings-with-prefix "C-c C-m")
;; eg. extract function with `C-c C-m ef`.
(add-hook 'js2-mode-hook #'js2-refactor-mode)))
(use-package tern
:ensure tern
:ensure tern-auto-complete
:config
(progn
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(tern-ac-setup)
))

;;(use-package jade
;;:ensure t
;;)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))


;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package company-web
  :after web-mode)

(use-package rjsx-mode
  :mode ("\\.jsx$" . rjsx-mode)
  :magic ("%React" . rjsx-mode))

(use-package vue-mode
  :mode
  ("\\.vue$" . vue-mode))

(use-package indium
  :after js2-mode
  :hook ((js2-mode . indium-interaction-mode))
  :bind (:map indium-interaction-mode-map
              ("C-x C-e" . indium-eval-last-node)
              ("C-<f6>" . vs/stop-indium-debug)
              ("S-<f6>" . indium-connect)
              ("<f6>" . indium-launch))
  :config (delight indium-interaction-mode))

(use-package mocha
  :init (setq mocha-reporter "spec")
  :bind (:map js2-mode-map
              (("C-c t" . mocha-test-project))))

(use-package json-mode
  :mode
  ("\\.json$" . json-mode))

  ;; (use-package tex
    ;; :ensure t)

  ;; (use-package cdlatex
  ;;   :ensure t)

  ;; ;;
  ;(use-package auctex
  ;;   :ensure t
  ;;   :config (setq TeX-auto-save t)
  ;;   (setq TeX-parse-self t)
  ;;   (setq TeX-close-quote "")
  ;;   (setq TeX-open-quote ""))


  ;; (defcustom
  ;;   prelude-latex-fast-math-entry 'LaTeX-math-mode
  ;;   "Method used for fast math symbol entry in LaTeX."
  ;;   :link '(function-link :tag "AUCTeX Math Mode" LaTeX-math-mode)
  ;;   :link '(emacs-commentary-link :tag "CDLaTeX" "cdlatex.el")
  ;;   :group 'prelude
  ;;   :type '(choice (const :tag "None" nil)
  ;;                  (const :tag "AUCTeX Math Mode" LaTeX-math-mode)
  ;; (const :tag "CDLaTeX" cdlatex)))

  ;; (defun tex-view ()
  ;;   (interactive)
  ;;   (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

  ;; (require 'latex-pretty-symbols)
  ;; (add-hook 'markdown-mode-hook 'pandoc-mode)
  ;; (add-hook 'markdown-mode-hook 'latex-unicode-simplified)
  ;; (setq markdown-enable-math 1)
  ;; (add-hook 'org-mode-hook 'latex-unicode-simplified)

  ;; (eval-after-load "tex"
  ;;   '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk")))
  ;; (eval-after-load "tex"
  ;;   '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk")))
  ;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

  (use-package geiser
    :ensure t
    :config (setq geiser-active-implementations '(guile racket)))

  (use-package ess
    :ensure t)

(use-package csv-mode
 :ensure t
:config
(setq csv-separators '("," ";" "|" " " )))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  (let ((plantuml-directory (concat user-emacs-directory "private/"))
        (plantuml-link "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))
    (let ((plantuml-target (concat plantuml-directory "plantuml.jar")))
      (if (not (file-exists-p plantuml-target))
          (progn (message "Downloading plantuml.jar")
                 (shell-command
                  (mapconcat 'identity (list "wget" plantuml-link "-O" plantuml-target) " "))
                 (kill-buffer "*Shell Command Output*")))
      (setq org-plantuml-jar-path plantuml-target
            plantuml-jar-path plantuml-target
            plantuml-output-type "svg"))))

(use-package flycheck-plantuml
  :config (flycheck-plantuml-setup))

  (defun ielm-auto-complete ()
    "Enables `auto-complete' support in \\[ielm]."
    (setq ac-sources '(ac-source-functions
                       ac-source-variables
                       ac-source-features
                       ac-source-symbols
                       ac-source-words-in-same-mode-buffers))
    ;; (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
    (auto-complete-mode 1))

  (defun ielm/clear-repl ()
    "Clear current REPL buffer."
    (interactive)
    (let ((inhibit-read-only t))
        (erase-buffer)
        (ielm-send-input)))

(use-package smartparens
  :diminish
  :init
  (define-key smartparens-mode-map (kbd "M-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-[") 'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-{") 'sp-wrap-curly)
  (progn
    (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
    (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
    (add-hook 'common-lisp-mode-hook #'smartparens-strict-mode)
    (add-hook 'scheme-mode-hook #'smartparens-strict-mode)
    (add-hook 'lisp-mode-hook #'smartparens-strict-mode))
    :config
    (require 'smartparens-config)
    (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
    (sp-local-pair '(common-lisp-mode) "'" "'" :actions nil)
    (sp-local-pair '(clojure-mode) "'" "'" :actions nil)
    (sp-local-pair '(cider-repl-mode) "'" "'" :actions nil)
    (sp-local-pair '(scheme-mode) "'" "'" :actions nil)
    (sp-local-pair '(lisp-mode) "'" "'" :actions nil)
    (setq smartparens-global-strict-mode 1))

  (use-package treemacs
    :ensure t
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
    (progn
      (setq treemacs-collapse-dirs   (if (executable-find "python") 3 0)
            treemacs-deferred-git-apply-delay      0.5
            treemacs-display-in-side-window        t
            treemacs-file-event-delay              5000
            treemacs-file-follow-delay             0.2
            treemacs-follow-after-init             t
            treemacs-git-command-pipe              ""
            treemacs-goto-tag-strategy             'refetch-index
            treemacs-indentation                   2
            treemacs-indentation-string            " "
            treemacs-is-never-other-window         nil
            treemacs-max-git-entries               5000
            treemacs-no-png-images                 nil
            treemacs-no-delete-other-windows       t
            treemacs-project-follow-cleanup        nil
            treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-recenter-distance             0.1
            treemacs-recenter-after-file-follow    nil
            treemacs-recenter-after-tag-follow     nil
            treemacs-recenter-after-project-jump   'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor                   nil
            treemacs-show-hidden-files             t
            treemacs-silent-filewatch              nil
            treemacs-silent-refresh                nil
            treemacs-sorting                       'alphabetic-desc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-width                         35)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (executable-find "python3"))))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))
    :bind
    (:map global-map
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

  (use-package treemacs-projectile
    :defer t
    :ensure t
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-heade))

  ;; (use-package treemacs-icons-dired
  ;;   :after treemacs dired
  ;;   :ensure t
  ;;   :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit
    :ensure t)
  ;(use-package treemacs-evil
   ; :ensure t)
  ;; (treemacs-reset-icons)

(use-package pretty-mode
  :ensure t
  :config
    (add-hook 'clojure-mode-hook #'turn-on-pretty-mode))

  (use-package ace-window
    :init
    (progn
      (setq aw-scope 'global) ;; was frame
      (global-set-key (kbd "C-x O") 'other-frame)
      (global-set-key [remap other-window] 'ace-window)
      (custom-set-faces
       '(aw-leading-char-face
         ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

  (use-package avy
    :bind
    ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

  (use-package counsel
    :bind
    (("M-y" . counsel-yank-pop)
     :map ivy-minibuffer-map
     ("M-y" . ivy-next-line)))

  (use-package ivy
    :diminish (ivy-mode)
    :bind (("C-x b" . ivy-switch-buffer))
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-display-style 'fancy))

  (use-package swiper
    :bind (("C-s" . swiper-isearch)
           ("C-r" . swiper-isearch)
           ("C-c C-r" . ivy-resume)
           ("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file))
    :config
    (progn
      (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      (setq ivy-display-style 'fancy)
      (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

  (setenv "GPG_AGENT_INFO" nil)
  (use-package epg
   :requires (epa-file password-cache)
   :config
   (setq epg-gpg-program "/usr/bin/gpg2")
   (setq password-cache-expiry (* 15 60))
   (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(use-package org
  :ensure org-plus-contrib
  :hook ((org-mode . toggle-word-wrap)
         (org-mode . org-indent-mode)
         (org-mode . turn-on-visual-line-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1))))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (setq ;org-default-notes-file (concat org-directory "~/sync/orgfiles/notes.org.gpg")
        org-export-html-postamble nil
        org-hide-leading-stars t
        org-startup-indented t
        org-journal-dir "~/sync/orgfiles"
        org-display-inline-images t
        org-redisplay-inline-images t
        org-startup-with-inline-images "inlineimages"
        ;org-agenda-files (list "~/sync/orgfiles/life.org.gpg" "~/sync/orgfiles/personal_cal.org.gpg" "~/sync/orgfiles/work_cal.org.gpg")
        org-todo-keywords
        '((sequence "TODO(t)" "PENDING(p!)" "WAIT(w@)" "VERIFY(v)" "|" "DONE(d!)" "CANCELED(c@)")
          (sequence "REPORT(r@)" "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(f!)"))
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-support-shift-select 'always
        org-hide-emphasis-markers        t
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively        t
        org-src-fontify-natively         t
        org-src-preserve-indentation     t)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (defun do-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))
  (global-set-key (kbd "C-c C-x C v")
                  'do-org-show-all-inline-images)
  (use-package ob-restclient)
  (use-package ob-ipython)
  (use-package ob-async
    :init (setq ob-async-no-async-languages-alist '("ipython")))
  (use-package ox-reveal)
  (org-babel-do-load-languages
           'org-babel-load-languages
           (org-babel-do-load-languages
            'org-babel-load-languages
            (append org-babel-load-languages
                    '((emacs-lisp . t)
                      (clojure . t)
                      (python . t)
                      (restclient . t)
                      (js . t)
                      (shell . t)
                      (plantuml . t)
                      (sql . t)
                      (ipython . t)))))
  (setq org-hide-emphasis-markers t
        org-babel-clojure-nrepl-timeout nil
        org-export-allow-bind-keywords t
        org-confirm-babel-evaluate       t)
  (org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
  (org-defkey org-mode-map "\C-c\C-d" 'cider-doc))

    ;; (use-package org-gcal
                 ;; :init (load-library "~/.gcal.el.gpg")
                 ;; :config (setq org-gcal-file-alist '(("maximoiann@gmail.com" .  "~/sync/orgfiles/personal_cal.org")
                                                     ;; ("ian@crowd.br.com" . "~/sync/orgfiles/work_cal.org"))))

  ;; organize journal confs after
   (load (expand-file-name (concat user-emacs-directory "sensitive/journal.el")))
   (use-package org-journal
     :init
     (defun org-journal-load-files ()
       (interactive)
       (when (not org-journal-loaded)
          (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]$")
          (add-to-list 'org-agenda-files org-journal-dir)
          (setq org-journal-loaded t)))
     :config (setq org-journal-loaded nil))

  (setq org-agenda-include-diary t)

   (require 'org-agenda)
   (setq org-agenda-include-diary t
          calendar-week-start-day 0
          calendar-day-name-array ["Domingo" "Segunda" "Terça" "Quarta"
                                   "Quinta" "Sexta" "Sábado"]
          calendar-month-name-array ["Janeiro" "Fevereiro" "Março" "Abril"
                                     "Maio" "Junho" "Julho" "Agosto"
                                     "Setembro" "Outubro" "Novembro" "Dezembro"])

   (add-to-list 'org-agenda-custom-commands
                 '("Y" "Agenda anual de aniversários e feriados" agenda "Visão Anual"
                   ((org-agenda-span 365)
                    (org-agenda-filter-by-category 'Aniversário)
                    (org-agenda-time-grid nil))))
   (add-to-list 'org-agenda-custom-commands
                 '("1" "Agenda mensal" agenda "Visão Mensal"
                   ((org-agenda-span 31)
                    (org-agenda-time-grid nil))))
   (add-to-list 'org-agenda-custom-commands
                 '("7" "Agenda dos próximos sete dias" agenda "Visão de Sete Dias"
                   ((org-agenda-span 7)
                    (org-agenda-time-grid nil))))

  (load (expand-file-name (concat user-emacs-directory "elisp/brazil-holidays.el")))
  (setq calendar-holidays holiday-brazil-all)

  (load (expand-file-name (concat user-emacs-directory "sensitive/agenda.el")))
  (add-hook 'org-mode-hook 'auto-revert-mode)

(use-package org-bullets
   :hook ((org-mode . org-bullets-mode))
   :init (setq org-hide-leading-stars t))

(use-package fill-column-indicator
   :config (progn
             (add-hook 'org-mode-hook
                       (lambda ()
                         (setq fci-rule-width 1)
                         (setq fci-rule-color "darkblue")))
             (add-hook 'org-mode-hook 'turn-on-auto-fill)))

(use-package org-alert
 :config (progn
           (setq alert-default-style          'libnotify
                 org-alert-notification-title "*org-mode*"
                 org-alert-interval           21600)
           (org-alert-enable)))

(use-package calfw)
(use-package calfw-org
 :requires calfw
 :config (progn
             (setq cfw:org-overwrite-default-keybinding t)
             (global-set-key (kbd "<f6>")
                             (lambda ()
                               (interactive)
                               (cfw:open-org-calendar)))))

(use-package org-re-reveal
  :init (setq org-re-reveal-root "https://cdn.jsdelivr.net/reveal.js/latest"
              org-reveal-mathjax t))

 (setq org-file-apps
   (append '(("\\.pdf\\'" . "evince %s")) org-file-apps))

 (require 'org-crypt)
 (org-crypt-use-before-save-magic)
 (setq org-tags-exclude-from-inheritance (quote ("crypt")))
 ;; GPG key to use for encryption
 ;; Either the Key ID or set to nil to use symmetric encryption.
 (setq org-crypt-key "9CD4DA20")

 (use-package org-web-tools
   :ensure t)

  (use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook)
    (progn (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
           (setq dashboard-center-content t)
           (setq dashboard-startup-banner 'logo)
           (setq dashboard-set-navigator t)
           (setq dashboard-items '((recents  . 5)
                                   (bookmarks . 5)
                                   (projects . 5)
                                   (agenda . 5)
                                   (registers . 5)))))

;; (setq
;;  backup-by-copying 1      ; don't clobber symlinks
;;  backup-directory-alist
;;  '(("." . "~/.saves"))    ; don't litter my fs tree
;;  delete-old-versions 1
;;  kept-new-versions 6
;;  kept-old-versions 2
;;  version-control 1)
                                        ; use versioned backups

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package auto-yasnippet
:ensure t)
(use-package yasnippet-snippets
  :after (yas-global-mode))

; mark and edit all copies of the marked region simultaniously.
(use-package iedit
:ensure t)

; if you're windened, narrow to the region, if you're narrowed, widen
; bound to C-x n
(defun narrow-or-widen-dwim (p)
"If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
(interactive "P")
(declare (interactive-only))
(cond ((and (buffer-narrowed-p) (not p)) (widen))
((region-active-p)
(narrow-to-region (region-beginning) (region-end)))
((derived-mode-p 'org-mode)
;; `org-edit-src-code' is not a real narrowing command.
;; Remove this first conditional if you don't want it.
(cond ((ignore-errors (org-edit-src-code))
(delete-other-windows))
((org-at-block-p)
(org-narrow-to-block))
(t (org-narrow-to-subtree))))
(t (narrow-to-defun))))

(use-package try
        :ensure t)

(use-package which-key
             :ensure t
             :config
             (which-key-mode))

  (use-package undo-tree
               :ensure t
               :init
               (setq global-undo-tree-mode 1))

  ;; (use-package evil
    ;; :ensure t
    ;; :init
    ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    ;; (setq evil-want-keybinding nil)
    ;; :config
    ;; (evil-mode 1)

  ;; (use-package evil-collection
    ;; :after evil
    ;; :ensure t
    ;; :config
    ;; (evil-collection-init)

  (use-package better-shell
      :ensure t
      :bind (("C-\"" . better-shell-shell)
             ("C-:" . better-shell-remote-open)))

  (use-package shell
    :ensure nil
    :commands comint-send-string comint-simple-send comint-strip-ctrl-m
    :preface
    (defun n-shell-simple-send (proc command)
      "Various PROC COMMANDs pre-processing before sending to shell."
      (cond
       ;; Checking for clear command and execute it.
       ((string-match "^[ \t]*clear[ \t]*$" command)
        (comint-send-string proc "\n")
        (erase-buffer))
       ;; Checking for man command and execute it.
       ((string-match "^[ \t]*man[ \t]*" command)
        (comint-send-string proc "\n")
        (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
        (setq command (replace-regexp-in-string "[ \t]+$" "" command))
        ;;(message (format "command %s command" command))
        (funcall 'man command))
       ;; Send other commands to the default handler.
       (t (comint-simple-send proc command))))
    (defun n-shell-mode-hook ()
      "Shell mode customizations."
      (local-set-key '[up] 'comint-previous-input)
      (local-set-key '[down] 'comint-next-input)
      (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
      (setq comint-input-sender 'n-shell-simple-send))
    :hook ((shell-mode . ansi-color-for-comint-mode-on)
           (shell-mode . n-shell-mode-hook))
    :config
    (setq system-uses-terminfo nil)       ; don't use system term info

    (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)

    ;; Company mode backend for shell functions
    (use-package company-shell
      :after company
      :init (cl-pushnew '(company-shell company-shell-env company-fish-shell)
                        company-backends))

    ;; Bash completion
    (use-package bash-completion
      :init (bash-completion-setup))

    ;; ANSI & XTERM 256 color support
    (use-package xterm-color
      :defines compilation-environment
      :init
      (setenv "TERM" "xterm-256color")
      (setq comint-output-filter-functions
            (remove 'ansi-color-process-output comint-output-filter-functions))

      (add-hook 'shell-mode-hook
                (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))))

(use-package keyfreq
  :ensure t
  :config
  (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(setq user-full-name "Ian Fernandez"
      user-mail-address "ianffcs@tutanota.com")
(global-set-key (kbd "<menu>")
                  (lambda () (interactive) (find-file "~/.emacs.d/init.org")))
  ;;--------------------------------------------------------------------------

  (use-package magit
    :ensure t
    :defer t
    :bind ("C-x g" . magit-status)
    :init
    (setq magit-diff-options (quote ("--word-diff")))
    (setq magit-diff-refine-hunk 'all)
    :config
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

  (use-package git-gutter
    :ensure t
    :init
    (global-git-gutter-mode +1))

    ;; Use evil keybindings within magit
  ;  (use-package evil-magit
  ;    :ensure t
  ;    :config
  ;    ;; Default commit editor opening in insert mode
  ;    (add-hook 'with-editor-mode-hook 'evil-insert-state)
  ;    (evil-define-key 'normal with-editor-mode-map
  ;      (kbd "RET") 'with-editor-finish
  ;      [escape] 'with-editor-cancel
  ;      )
  ;    (evil-define-key 'normal git-rebase-mode-map
  ;      "l" 'git-rebase-show-commit))

(use-package pdf-tools
  :ensure t
  :pin manual
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

(use-package org-pdfview
:ensure t)

(use-package projectile
      :ensure t
      :bind ("C-c p" . projectile-command-map)
      :config
      (projectile-global-mode)
    (setq projectile-completion-system 'ivy))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (or

                        (mode . mu4e-compose-mode)
                        (name . "\*mu4e\*")
                        ))
               ("programming" (or
                               (mode . clojure-mode)
                               (mode . clojurescript-mode)
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
                                        ;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)

(use-package pcre2el
:ensure t
:config (pcre-mode))

(setq counsel-spotify-client-id "ab61a7718cc1467eb8fbd6a374a5eb3a")
(setq counsel-spotify-client-secret "825f638e071d445287e36369c4075130")
(use-package counsel-spotify
:ensure t
:config
(require 'counsel-spotify)
)

  ;; (use-package emms
  ;;   :ensure t
  ;;   :config
  ;;   (require 'emms-setup)
  ;;   (require 'emms-player-mpd)
  ;;   (emms-all)
  ;;   (setq emms-seek-seconds 5)
  ;;   (setq emms-player-list '(emms-player-mpd))
  ;;   (setq emms-info-functions '(emms-info-mpd))
  ;;   (setq emms-player-mpd-server-name "localhost")
  ;;   (setq emms-player-mpd-server-port "6601")
  ;;   (setq emms-playlist-buffer-name "*Music*")
  ;;   (setq emms-info-asynchronously t)
  ;;   (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  ;;   (setq emms-info-functions '(emms-info-libtag)) ;;; make sure libtag is the only thing delivering metadata
  ;;   (require 'emms-mode-line)
  ;;   (emms-mode-line 1)
  ;;   (require 'emms-playing-time)
  ;;   (emms-playing-time 1)
  ;;   :bind
  ;;   ("s-m p" . emms)
  ;;   ("s-m b" . emms-smart-browse)
  ;;   ("s-m r" . emms-player-mpd-update-all-reset-cache)
  ;;   ("<XF86AudioPrev>" . emms-previous)
  ;;   ("<XF86AudioNext>" . emms-next)
  ;;   ("<XF86AudioPlay>" . emms-pause)
  ;;   ("<XF86AudioStop>" . emms-stop))

  ;; (defun ts/showsong ()
  ;;  (emms-next-noerror)
  ;;  (set 'notifyid (dbus-call-method :session "org.kde.knotify" "/Notify" "org.kde.KNotify" "event" "emms_song" "emacs" '(:array (:variant nil)) "Currently Playing" (emms-show) '(:array :byte 0 :byte 0 :byte 0 :byte 0) '(:array) :int64 0))
  ;;  (run-at-time "5 sec" nil 'dbus-call-method :session "org.kde.knotify" "/Notify" "org.kde.KNotify" "closeNotification" :int32 notifyid)
  ;;  )

  ;; (setq emms-player-next-function 'ts/showsong)

  ;; (defun mpd/start-music-daemon ()
  ;; "Start MPD, connects to it and syncs the metadata cache."
  ;; (interactive)
  ;; (shell-command "mpd")
  ;; (mpd/update-database)
  ;; (emms-player-mpd-connect)
  ;; (emms-cache-set-from-mpd-all)
  ;; (message "MPD Started!"))
  ;; (global-set-key (kbd "s-m c") 'mpd/start-music-daemon)

  ;; (defun mpd/kill-music-daemon ()
  ;; "Stops playback and kill the music daemon."
  ;; (interactive)
  ;; (emms-stop)
  ;; (call-process "killall" nil nil nil "mpd")
  ;; (message "MPD Killed!"))
  ;; (global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)

  ;; (defun mpd/update-database ()
  ;; "Updates the MPD database synchronously."
  ;; (interactive)
  ;; (call-process "mpc" nil nil nil "update")
  ;; (message "MPD Database Updated!"))
  ;; (global-set-key (kbd "s-m u") 'mpd/update-database)

(setq mpc-host "localhost:6601")

(use-package telega
  :load-path  "~/telega.el"
  :commands (telega)
  :defer t)

  (use-package wakatime-mode
    :ensure t
    :config
    (setq wakatime-api-key "73d4ae10-c5e3-490e-816e-0976c22ecd22"))
