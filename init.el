;; This file replaces itself with the actual configuration at first run.

;; ;; We can't tangle without org!
;; (require 'org)
;; ;; Open the configuration
;; (find-file (concat user-emacs-directory "init.org"))
;; ;; tangle it
;; (org-babel-tangle)
;; ;; load it
;; (load-file (concat user-emacs-directory "init.el"))
;; ;; finally byte-compile it
;; (byte-compile-file (concat user-emacs-directory "init.el"))

;;;; Initialization

;;  (defun tangle-init ()
;;    "If the current buffer is 'init.org' the code-blocks are
;;    tangled, and the tangled file is compiled."
;;    (when (equal (buffer-file-name)
;; 		 (expand-file-name (concat user-emacs-directory "init.org")))
;;      ;; Avoid running hooks when tangling.
;;      (let ((prog-mode-hook nil))
;; 	(org-babel-tangle)
;; 	(byte-compile-file (concat user-emacs-directory "init.el")))))

;; (add-hook 'after-save-hook 'tangle-init)

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
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
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

(defconst *is-a-mac* (eq system-type 'darwin))

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

(mode-icons-mode)


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

(global-wakatime-mode)

(defalias 'yes-or-no-p 'y-or-n-p)
;; before save clears whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "C-c i") 'string-inflection-all-cycle)

;; (defconst my-default-font "-*-fixed-medium-r-normal-*-15-*-*-*-*-*-*-*")
;; (defconst my-default-font "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*")
;; (defconst my-default-font "-b&h-lucidatypewriter-medium-r-normal-sans-14-*-*-*-*-*-iso8859-1")
;; (defconst my-default-font "FantasqueSansMono Nerd Font-10")
;; (defconst my-default-font "Monoid-9")
;; (defconst my-default-font "Fixed-10")
(defconst my-default-font "Dina-10")
;; (defconst my-default-font "Iosevka-9")
;; (defconst my-default-font "Terminus-10")
;; (defconst my-default-font "Hack-10")

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
      blink-cursor-mode             0
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
    :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(add-hook 'prog-mode-hook 'linum-mode)

(defun set-frame-alpha (value)
  "Set the transparency of the frame. 0 = transparent/100 = opaque"
  (interactive "Alpha value (0-100): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(set-frame-alpha 90)

(setenv "GPG_AGENT_INFO" nil)

(setq epg-gpg-program "/usr/bin/gpg2")

(require 'epa-file)

(require 'password-cache)

(setq password-cache-expiry (* 15 60))

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(use-package org
  :ensure t
  ;:pin org
  :config
  '(org-directory "~/sync/orgfiles")
  '(org-default-notes-file (concat org-directory "~/sync/orgfiles/notes.org.gpg"))
  '(org-export-html-postamble nil)
  '(org-hide-leading-stars t)
  '(org-startup-indented t)
  '(org-journal-dir "~/sync/orgfiles")
  '(org-agenda-files (list "~/sync/orgfiles/life.org.gpg" "~/sync/orgfiles/personal_cal.org.gpg" "~/sync/orgfiles/work_cal.org.gpg"))
  '(org-display-inline-images t)
  '(org-redisplay-inline-images t)
  '(org-startup-with-inline-images "inlineimages")
  '(set-default 'preview-scale-function 2.0)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PENDING(p!)" "WAIT(w@)" "VERIFY(v)" "|" "DONE(d!)" "CANCELED(c@)")
          (sequence "REPORT(r@)" "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(f!)")))
  (add-hook 'org-mode-hook (lambda ()
                             (setq truncate-lines nil)
                             (visual-line-mode)
                             (org-ident-mode)))
  ;; (global-set-key (kbd "C-c l")
  ;;               (lambda () (interactive) (find-file "~/sync/orgfiles/life.org.gpg")))
  (add-hook 'org-mode-hook #'toggle-word-wrap)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (defun do-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))
  (global-set-key (kbd "C-c C-x C v")
                  'do-org-show-all-inline-images)
  (require 'ox-reveal))

;; (use-package org-gcal
             ;; :init (load-library "~/.gcal.el.gpg")
             ;; :config (setq org-gcal-file-alist '(("maximoiann@gmail.com" .  "~/sync/orgfiles/personal_cal.org")
                                                 ;; ("ian@crowd.br.com" . "~/sync/orgfiles/work_cal.org"))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-reveal-root "file:///home/ianffcs/reveal.js")

(setq org-file-apps
  (append '(
             ("\\.pdf\\'" . "evince %s")
             ("\\.x?html?\\'" . "/usr/bin/chromium-browser %s")) org-file-apps))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "9CD4DA20")

(use-package org-web-tools
  :ensure t)

(use-package org-journal
  :ensure t)

(org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (emacs-lisp . t)
                               (shell . t)
                               (plantuml . t)
                               (C . t)
                               ;; (Clojure . t)
                               (haskell . t)
                               (R . t)
                               (js . t)
                               (dot . t)
                               (org . t)
                               (latex . t )))

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

(setq
   backup-by-copying 1      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions 1
   kept-new-versions 6
   kept-old-versions 2
   version-control 1)       ; use versioned backups

(use-package counsel
  :ensure t
  :bind*
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 7)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package smex
  :ensure t)

(use-package avy
:ensure t
:bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

(use-package ace-jump-mode
  :ensure t
  :config
  (autoload
    'ace-jump-mode
    "ace-jump-mode"
    "Emacs quick move minor mode"
    t)
;; you can select the key you prefer to
  (define-key global-map (kbd "<f9>") 'ace-jump-mode)
  (autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-<f9>") 'ace-jump-mode-pop-mark))

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

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(use-package company-jedi
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
       )

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-joker
  :ensure t)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))
(use-package auto-yasnippet
:ensure t)

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
             (global-undo-tree-mode))

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

(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6601")
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t)
  (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  (setq emms-info-functions '(emms-info-libtag)) ;;; make sure libtag is the only thing delivering metadata
  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1)
  :bind
  ("s-m p" . emms)
  ("s-m b" . emms-smart-browse)
  ("s-m r" . emms-player-mpd-update-all-reset-cache)
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioStop>" . emms-stop))

(defun ts/showsong ()
 (emms-next-noerror)
 (set 'notifyid (dbus-call-method :session "org.kde.knotify" "/Notify" "org.kde.KNotify" "event" "emms_song" "emacs" '(:array (:variant nil)) "Currently Playing" (emms-show) '(:array :byte 0 :byte 0 :byte 0 :byte 0) '(:array) :int64 0))
 (run-at-time "5 sec" nil 'dbus-call-method :session "org.kde.knotify" "/Notify" "org.kde.KNotify" "closeNotification" :int32 notifyid)
 )

(setq emms-player-next-function 'ts/showsong)

(defun mpd/start-music-daemon ()
"Start MPD, connects to it and syncs the metadata cache."
(interactive)
(shell-command "mpd")
(mpd/update-database)
(emms-player-mpd-connect)
(emms-cache-set-from-mpd-all)
(message "MPD Started!"))
(global-set-key (kbd "s-m c") 'mpd/start-music-daemon)

(defun mpd/kill-music-daemon ()
"Stops playback and kill the music daemon."
(interactive)
(emms-stop)
(call-process "killall" nil nil nil "mpd")
(message "MPD Killed!"))
(global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)

(defun mpd/update-database ()
"Updates the MPD database synchronously."
(interactive)
(call-process "mpc" nil nil nil "update")
(message "MPD Database Updated!"))
(global-set-key (kbd "s-m u") 'mpd/update-database)

(setq mpc-host "localhost:6601")

(use-package telega
  :load-path  "~/telega.el"
  :commands (telega)
  :defer t)

(use-package wakatime-mode
  :ensure t
  :config
  (setq wakatime-api-key "73d4ae10-c5e3-490e-816e-0976c22ecd22"))

(use-package smartparens
  :ensure t
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
    (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
    (smartparens-global-strict-mode))
    :config
    (require 'smartparens-config)
    (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
    (sp-local-pair '(common-lisp-mode) "'" "'" :actions nil)
    (sp-local-pair '(clojure-mode) "'" "'" :actions nil)
    (sp-local-pair '(cider-repl-mode) "'" "'" :actions nil)
    (sp-local-pair '(scheme-mode) "'" "'" :actions nil)
    (sp-local-pair '(lisp-mode) "'" "'" :actions nil))

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

(setq py-python-command "python3")
(setq python-shell-interpreter "python3")


    (use-package elpy
    :ensure t
    :config
    (elpy-enable))

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

  (use-package ein
    :ensure t)

(use-package hy-mode
  :ensure t)

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :bind ( :map rust-mode-map
         (("C-c C-t" . racer-describe)))
  :config
  (progn
    ;; add flycheck support for rust
    ;; https://github.com/flycheck/flycheck-rust
    (use-package flycheck-rust)

    ;; cargo-mode for all the cargo related operations
    ;; https://github.com/kwrooijen/cargo.el
    (use-package cargo)

    ;; racer-mode for getting IDE like features for rust-mode
    ;; https://github.com/racer-rust/emacs-racer
    (use-package racer
      :config
      (progn
        ;; set racer rust source path environment variable
        (setq racer-rust-src-path "/home/ianffcs/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
        (defun my-racer-mode-hook ()
          (set (make-local-variable 'company-backends)
               '((company-capf company-files))))

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
                  (rust-format-buffer))))))

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

;; (use-package tex
  ;; :ensure t)

(use-package cdlatex
  :ensure t)

;; (use-package auctex
;;   :ensure t
;;   :config (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-close-quote "")
;;   (setq TeX-open-quote ""))


(defcustom
  prelude-latex-fast-math-entry 'LaTeX-math-mode
  "Method used for fast math symbol entry in LaTeX."
  :link '(function-link :tag "AUCTeX Math Mode" LaTeX-math-mode)
  :link '(emacs-commentary-link :tag "CDLaTeX" "cdlatex.el")
  :group 'prelude
  :type '(choice (const :tag "None" nil)
                 (const :tag "AUCTeX Math Mode" LaTeX-math-mode)
(const :tag "CDLaTeX" cdlatex)))

(defun tex-view ()
  (interactive)
  (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

(require 'latex-pretty-symbols)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'latex-unicode-simplified)
(setq markdown-enable-math 1)
(add-hook 'org-mode-hook 'latex-unicode-simplified)

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk")))
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk")))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

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
  :ensure t
  :config
  (setq org-plantuml-jar-path
       (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (setq plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes
               '("plantuml" . plantuml))
  (add-to-list 'auto-mode-alist
               '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist
               '("\\.ptuml\\'" . plantuml-mode)))

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

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'common-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)))
