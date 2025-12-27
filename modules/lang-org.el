;;; lang-org.el --- Org-mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive Org-mode configuration:
;; - Core org-mode settings
;; - Agenda and capture
;; - Babel (code blocks)
;; - Export (HTML, Markdown, Reveal.js)
;; - Org-roam (Zettelkasten)
;; - Visual enhancements (org-modern, org-appear)
;; - Extensions (pomodoro, journal, super-agenda)
;;
;; Split from lang-text.el for better organization.

;;; Code:

;; ============================================================================
;; 1. ORG-MODE CORE
;; ============================================================================

(use-package org
  :straight (:type built-in)
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . flyspell-mode)
         (org-mode . ian/org-mode-setup))
  :custom
  ;; --- Directories ---
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))

  ;; --- Appearance ---
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(600))
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)

  ;; --- Editing ---
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-link-descriptive t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-M-RET-may-split-line nil)
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'smart)
  (org-cycle-separator-lines 0)
  (org-list-allow-alphabetical t)
  (org-use-speed-commands t)

  ;; --- Source Blocks ---
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)

  ;; --- TODO & Tags ---
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "red" :weight bold))
     ("NEXT" . (:foreground "orange" :weight bold))
     ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
     ("WAITING" . (:foreground "purple" :weight bold))
     ("HOLD" . (:foreground "magenta" :weight bold))
     ("DONE" . (:foreground "green" :weight bold))
     ("CANCELLED" . (:foreground "gray" :weight bold))))
  (org-use-fast-todo-selection 'expert)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)

  ;; --- Tags ---
  (org-tag-alist '((:startgroup)
                   ("@home" . ?h)
                   ("@work" . ?w)
                   ("@errand" . ?e)
                   (:endgroup)
                   ("project" . ?p)
                   ("meeting" . ?m)
                   ("note" . ?n)
                   ("idea" . ?i)
                   ("urgent" . ?u)
                   ("recurring" . ?r)))
  (org-tags-column -80)
  (org-fast-tag-selection-single-key 'expert)

  ;; --- Priorities ---
  (org-priority-highest ?A)
  (org-priority-lowest ?D)
  (org-priority-default ?C)
  (org-priority-faces
   '((?A . (:foreground "red" :weight bold))
     (?B . (:foreground "orange" :weight bold))
     (?C . (:foreground "yellow" :weight bold))
     (?D . (:foreground "green" :weight bold))))

  ;; --- Clocking ---
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-when-done t)
  (org-clock-report-include-clocking-task t)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)

  ;; --- Archiving ---
  (org-archive-location "%s_archive::datetree/")

  ;; --- Refiling ---
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  :config

  (defun ian/org-mode-setup ()
    "Custom org mode setup."
    (setq-local fill-column 80)
    (setq-local truncate-lines nil))

  ;; Enable org-clock persistence
  (org-clock-persistence-insinuate)

  ;; Refile to top-level headings
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  ;; Speed keys at beginning of headlines
  (setq org-speed-commands
        '(("Outline Navigation")
          ("n" . org-next-visible-heading)
          ("p" . org-previous-visible-heading)
          ("f" . org-forward-heading-same-level)
          ("b" . org-backward-heading-same-level)
          ("u" . outline-up-heading)
          ("j" . org-goto)
          ("g" . org-goto)
          ("Outline Visibility")
          ("c" . org-cycle)
          ("C" . org-shifttab)
          ("Outline Structure Editing")
          ("U" . org-metaup)
          ("D" . org-metadown)
          ("r" . org-metaright)
          ("l" . org-metaleft)
          ("R" . org-shiftmetaright)
          ("L" . org-shiftmetaleft)
          ("i" . org-insert-heading-respect-content)
          ("^" . org-sort)
          ("w" . org-refile)
          ("a" . org-archive-subtree-default-with-confirmation)
          ("@" . org-mark-subtree)
          ("#" . org-toggle-comment)
          ("Clock Commands")
          ("I" . org-clock-in)
          ("O" . org-clock-out)
          ("Meta Data Editing")
          ("t" . org-todo)
          ("," . org-priority)
          ("0" . (org-priority ?\ ))
          ("1" . (org-priority ?A))
          ("2" . (org-priority ?B))
          ("3" . (org-priority ?C))
          (":" . org-set-tags-command)
          ("e" . org-set-effort)
          ("E" . org-inc-effort)
          ("Agenda Views")
          ("v" . org-agenda)
          ("/" . org-sparse-tree))))

;; ============================================================================
;; 2. ORG AGENDA
;; ============================================================================

(use-package org-agenda
  :straight (:type built-in)
  :after org
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday 1)
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-include-diary t)
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string "◀── now ────────────────────────────────")
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-hide-tags-regexp ".")
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  :config
  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "In Progress")))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting On")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("w" "Work Tasks"
           ((agenda "" ((org-agenda-span 'day)))
            (tags-todo "+@work"
                       ((org-agenda-overriding-header "Work Tasks")))))

          ("h" "Home Tasks"
           ((tags-todo "+@home"
                       ((org-agenda-overriding-header "Home Tasks")))))

          ("p" "Projects"
           ((tags "project"
                  ((org-agenda-overriding-header "Projects")))))

          ("W" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "DONE"
                  ((org-agenda-overriding-header "Completed Tasks")))
            (todo "CANCELLED"
                  ((org-agenda-overriding-header "Cancelled Tasks"))))))))

;; ============================================================================
;; 3. ORG CAPTURE
;; ============================================================================

(use-package org-capture
  :straight (:type built-in)
  :after org
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %? :note:\n%U\n%a\n" :clock-in t :clock-resume t)

     ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings")
      "* %? :meeting:\n%U\n** Attendees\n- \n** Notes\n- \n** Action Items\n- [ ] " :clock-in t :clock-resume t)

     ("j" "Journal" entry (file+olp+datetree "journal.org")
      "* %?\n%U\n" :clock-in t :clock-resume t)

     ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
      "* %? :idea:\n%U\n" :clock-in t :clock-resume t)

     ("l" "Link" entry (file+headline org-default-notes-file "Links")
      "* %? :link:\n%U\n%a\n" :clock-in t :clock-resume t)

     ("b" "Book" entry (file+headline "reading.org" "Books")
      "* %^{Title}\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:GENRE: %^{Genre}\n:END:\n%U\n%?" :clock-in t :clock-resume t)

     ("p" "Protocol" entry (file+headline org-default-notes-file "Web")
      "* %^{Title}\n:PROPERTIES:\n:SOURCE: %u, %c\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?" :clock-in t :clock-resume t)

     ("w" "Weekly Review" entry (file+olp+datetree "reviews.org")
      "* Weekly Review %U\n** What went well?\n- %?\n** What could be improved?\n- \n** Goals for next week\n- [ ] " :clock-in t :clock-resume t))))

;; ============================================================================
;; 4. ORG BABEL
;; ============================================================================

(use-package ob
  :straight (:type built-in)
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (ruby . t)
     (js . t)
     (sql . t)
     (sqlite . t)
     (css . t)
     (sass . t)
     (plantuml . t)
     (mermaid . t)
     (dot . t)
     (gnuplot . t)
     (R . t)
     (latex . t)
     (org . t)
     (makefile . t)
     (hy . t)
     (clojure . t)
     (scheme . t)
     (lisp . t)
     (restclient . t)))

  ;; Don't ask for confirmation
  (setq org-confirm-babel-evaluate nil)

  ;; Default header arguments
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no"))))

;; Async babel execution
(use-package ob-async
  :after ob)

;; Go babel support
(use-package ob-go
  :after ob)

;; Kotlin babel support
(use-package ob-kotlin
  :after ob)

;; Racket babel support
(use-package ob-racket
  :straight (:host github :repo "hasu/emacs-ob-racket")
  :after ob
  :config
  (add-to-list 'org-babel-load-languages '(racket . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; ============================================================================
;; 5. ORG EXPORT
;; ============================================================================

(use-package ox
  :straight (:type built-in)
  :after org
  :custom
  (org-export-with-toc t)
  (org-export-with-section-numbers nil)
  (org-export-with-smart-quotes t)
  (org-export-headline-levels 6)
  (org-export-with-sub-superscripts '{})
  (org-export-use-babel t))

;; HTML export
(use-package ox-html
  :straight (:type built-in)
  :after ox
  :custom
  (org-html-validation-link nil)
  (org-html-head-include-scripts nil)
  (org-html-head-include-default-style nil)
  (org-html-doctype "html5")
  (org-html-html5-fancy t))

;; Markdown export
(use-package ox-md
  :straight (:type built-in)
  :after ox)

;; Htmlize - syntax highlighting for HTML export
(use-package htmlize
  :after ox)

;; GitHub Flavored Markdown
(use-package ox-gfm
  :after ox)

;; Hugo blog export
(use-package ox-hugo
  :after ox)

;; Pandoc export
(use-package ox-pandoc
  :after ox)

;; ============================================================================
;; 6. REVEAL.JS PRESENTATIONS
;; ============================================================================

(use-package emacs-reveal
  :straight (:host gitlab :repo "oer/emacs-reveal")
  :after org
  :custom
  (oer-reveal-revealjs-version "4")
  (oer-reveal-plugin-4-config
   "  plugins: [ RevealMarkdown, RevealHighlight, RevealNotes, RevealSearch, RevealZoom, RevealMath.KaTeX ],")
  (oer-reveal-master "~/.emacs.d/reveal.js")
  (oer-reveal-export-dir "./public")
  (oer-reveal-single-file nil)
  (oer-reveal-slide-width 1920)
  (oer-reveal-slide-height 1080)
  (oer-reveal-margin "0.1")
  (oer-reveal-min-scale "0.2")
  (oer-reveal-max-scale "1.5")
  (oer-reveal-transition "slide")
  (oer-reveal-transition-speed "default")
  (oer-reveal-with-notes t)
  (oer-reveal-with-toc nil)
  (oer-reveal-with-date nil)
  (oer-reveal-hlevel 2)
  (oer-reveal-highlight-theme "monokai")
  :config
  (require 'oer-reveal)
  (setq oer-reveal-theme "white")
  (setq oer-reveal-notes-popup t)
  (setq oer-reveal-audio-slideshow-config
        " audio: {
           prefix: 'audio/',
           suffix: '.ogg',
           advance: -1,
           autoplay: false,
           defaultDuration: 5,
           playerOpacity: 0.05,
         },"))

(use-package epresent
  :commands epresent-run)

;; OER Reveal
(use-package oer-reveal
  :straight (:host gitlab :repo "oer/oer-reveal")
  :after emacs-reveal
  :custom
  (oer-reveal-figures-dir "./figures")
  (oer-reveal-figure-default-width "80%")
  (oer-reveal-with-bibliography t)
  (oer-reveal-bibliography-style "apa")
  (oer-reveal-license "CC-BY-SA-4.0")
  (oer-reveal-license-url "https://creativecommons.org/licenses/by-sa/4.0/")
  (oer-reveal-default-grid-class "grid-wrapper")
  :config
  (require 'oer-reveal-publish))

;; ox-reveal as fallback
(use-package ox-reveal
  :after ox
  :custom
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-reveal-theme "white")
  (org-reveal-transition "slide")
  (org-reveal-plugins '(notes search zoom)))

;; --- Reveal.js Helper Functions ---
(defun ian/reveal-export-to-html ()
  "Export current org buffer to reveal.js HTML."
  (interactive)
  (require 'oer-reveal)
  (oer-reveal-export-to-html))

(defun ian/reveal-export-to-html-and-open ()
  "Export to reveal.js HTML and open in browser."
  (interactive)
  (ian/reveal-export-to-html)
  (let ((html-file (concat (file-name-sans-extension (buffer-file-name)) ".html")))
    (browse-url-of-file html-file)))

(defun ian/reveal-insert-slide ()
  "Insert a new slide heading."
  (interactive)
  (org-insert-heading)
  (insert " "))

(defun ian/reveal-insert-notes ()
  "Insert speaker notes block."
  (interactive)
  (insert "#+BEGIN_NOTES\n\n#+END_NOTES")
  (forward-line -1))

(defun ian/reveal-insert-fragment ()
  "Insert a fragment (incremental reveal) block."
  (interactive)
  (insert "#+ATTR_REVEAL: :frag (appear)"))

(defun ian/reveal-insert-two-columns ()
  "Insert a two-column layout."
  (interactive)
  (insert "#+REVEAL_HTML: <div class=\"column\" style=\"float:left; width: 50%\">\n\n#+REVEAL_HTML: </div>\n\n#+REVEAL_HTML: <div class=\"column\" style=\"float:right; width: 50%\">\n\n#+REVEAL_HTML: </div>"))

(defun ian/reveal-insert-background ()
  "Insert slide background property."
  (interactive)
  (let ((bg-type (completing-read "Background type: " '("color" "image" "video" "iframe"))))
    (pcase bg-type
      ("color" (insert ":PROPERTIES:\n:reveal_background: #1a1a1a\n:END:\n"))
      ("image" (insert ":PROPERTIES:\n:reveal_background: ./images/background.jpg\n:reveal_background_size: cover\n:END:\n"))
      ("video" (insert ":PROPERTIES:\n:reveal_background_video: ./video/background.mp4\n:reveal_background_video_loop: t\n:reveal_background_video_muted: t\n:END:\n"))
      ("iframe" (insert ":PROPERTIES:\n:reveal_background_iframe: https://example.com\n:END:\n")))))

(defun ian/reveal-new-presentation ()
  "Create a new reveal.js presentation with template."
  (interactive)
  (let ((filename (read-file-name "Presentation file: " nil nil nil "presentation.org")))
    (find-file filename)
    (insert "#+TITLE: My Presentation
#+SUBTITLE: Subtitle Here
#+AUTHOR: " user-full-name "
#+EMAIL: " user-mail-address "
#+DATE: " (format-time-string "%Y-%m-%d") "
#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js
#+REVEAL_THEME: white
#+REVEAL_TRANS: slide
#+REVEAL_PLUGINS: (notes search zoom)
#+REVEAL_EXTRA_CSS: ./custom.css
#+REVEAL_TITLE_SLIDE: <h1>%t</h1><h3>%s</h3><p>%a</p><p>%d</p>
#+OPTIONS: toc:nil num:nil reveal_title_slide:t
#+REVEAL_INIT_OPTIONS: slideNumber:true, hash:true, history:true, center:true

* Introduction
:PROPERTIES:
:reveal_background: #2d2d2d
:END:

Welcome to my presentation!

#+BEGIN_NOTES
Speaker notes go here.
#+END_NOTES

* Section 1

** Slide 1.1

- Point one
- Point two
- Point three

** Slide 1.2

#+ATTR_REVEAL: :frag (appear)
- This appears first
- This appears second
- This appears third

* Conclusion

** Summary

Thank you!

** Questions?

")
    (goto-char (point-min))
    (save-buffer)
    (message "Created new presentation: %s" filename)))

;; Keybindings for reveal.js
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r e") #'ian/reveal-export-to-html)
  (define-key org-mode-map (kbd "C-c r E") #'ian/reveal-export-to-html-and-open)
  (define-key org-mode-map (kbd "C-c r s") #'ian/reveal-insert-slide)
  (define-key org-mode-map (kbd "C-c r n") #'ian/reveal-insert-notes)
  (define-key org-mode-map (kbd "C-c r f") #'ian/reveal-insert-fragment)
  (define-key org-mode-map (kbd "C-c r c") #'ian/reveal-insert-two-columns)
  (define-key org-mode-map (kbd "C-c r b") #'ian/reveal-insert-background)
  (define-key org-mode-map (kbd "C-c r N") #'ian/reveal-new-presentation))

;; --- Transient Menu for Reveal.js ---
(with-eval-after-load 'transient
  (transient-define-prefix ian/reveal-menu ()
                           "Reveal.js presentation commands"
                           ["Export"
                            ("e" "Export to HTML" ian/reveal-export-to-html)
                            ("E" "Export and open" ian/reveal-export-to-html-and-open)]
                           ["Insert"
                            ("s" "New slide" ian/reveal-insert-slide)
                            ("n" "Speaker notes" ian/reveal-insert-notes)
                            ("f" "Fragment" ian/reveal-insert-fragment)
                            ("c" "Two columns" ian/reveal-insert-two-columns)
                            ("b" "Background" ian/reveal-insert-background)]
                           ["Create"
                            ("N" "New presentation" ian/reveal-new-presentation)])

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c r r") #'ian/reveal-menu)))

;; ============================================================================
;; 7. ORG VISUAL ENHANCEMENTS
;; ============================================================================

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✦"))
  (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
  (org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐")))
  (org-modern-tag t)
  (org-modern-priority t)
  (org-modern-todo t)
  (org-modern-table t)
  (org-modern-block-fringe nil)
  (org-modern-block-name
   '((t . t)
     ("src" "»" "«")
     ("example" "»–" "–«")
     ("quote" "❝" "❞"))))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

;; ============================================================================
;; 8. ORG-ROAM (Zettelkasten)
;; ============================================================================

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :reference:\n\n* Source\n\n* Notes\n\n")
      :unnarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n\n* Goals\n\n* Tasks\n\n* Notes\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n\n"))))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n D" . org-roam-dailies-capture-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n T" . org-roam-dailies-goto-tomorrow)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

(use-package deft
  :after org-roam
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; Org-roam UI
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;; ============================================================================
;; 9. ORG EXTENSIONS
;; ============================================================================

;; --- Org Download (Paste images) ---
(use-package org-download
  :after org
  :hook ((org-mode . org-download-enable)
         (dired-mode . org-download-enable))
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (expand-file-name "images" org-directory))
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-screenshot-method
   (cond ((eq system-type 'darwin) "screencapture -i %s")
         ((eq system-type 'gnu/linux) "scrot -s %s"))))

;; --- Org Cliplink (Paste links with titles) ---
(use-package org-cliplink
  :after org
  :bind (:map org-mode-map
              ("C-c C-l" . org-cliplink)))

;; --- Org Web Tools (Fetch web content) ---
(use-package org-web-tools
  :after org
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org))

;; --- Org Ref (Citations and bibliography) ---
(use-package org-ref
  :after org
  :custom
  (org-ref-default-bibliography '("~/org/bibliography/references.bib"))
  (org-ref-pdf-directory "~/org/bibliography/pdfs/")
  (org-ref-notes-directory "~/org/bibliography/notes/"))

;; --- Org Pomodoro (Time management) ---
(use-package org-pomodoro
  :after org
  :bind (:map org-mode-map
              ("C-c o p" . org-pomodoro))
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 15)
  (org-pomodoro-long-break-frequency 4))

(use-package org-alert
  :after org
  :custom
  (org-alert-interval 300)
  (org-alert-notification-title "Org Reminder")
  :config
  (org-alert-enable))

(use-package org-journal
  :after org
  :bind (("C-c o j" . org-journal-new-entry))
  :custom
  (org-journal-dir (expand-file-name "journal" org-directory))
  (org-journal-date-format "%Y-%m-%d (%A)")
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-enable-encryption nil))

;; --- Org Super Agenda (Better agenda views) ---
(use-package org-super-agenda
  :after org-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '((:name "Today"
            :time-grid t
            :date today
            :scheduled today
            :order 1)
     (:name "Due Today"
            :deadline today
            :order 2)
     (:name "Important"
            :priority "A"
            :order 3)
     (:name "Overdue"
            :deadline past
            :order 4)
     (:name "Due Soon"
            :deadline future
            :order 5)
     (:name "Next"
            :todo "NEXT"
            :order 6)
     (:name "In Progress"
            :todo "IN-PROGRESS"
            :order 7)
     (:name "Waiting"
            :todo "WAITING"
            :order 8)
     (:discard (:anything t)))))

;; --- Org QL (Query language for org) ---
(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-view))

;; --- TOC Org (Auto-generate TOC) ---
(use-package toc-org
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode)))

;; --- Org Fragtog (Auto-toggle LaTeX fragments) ---
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; --- Org Tempo (Quick templates) ---
(use-package org-tempo
  :straight (:type built-in)
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; --- Org Auto Tangle ---
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; --- Denote (Simple note-taking alternative to Roam) ---
(use-package denote
  :custom
  (denote-directory (expand-file-name "notes" org-directory))
  (denote-known-keywords '("emacs" "project" "reference" "idea"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  :bind (("C-c N n" . denote)
         ("C-c N N" . denote-type)
         ("C-c N d" . denote-date)
         ("C-c N s" . denote-subdirectory)
         ("C-c N t" . denote-template)
         ("C-c N r" . denote-rename-file)
         ("C-c N l" . denote-link)
         ("C-c N L" . denote-link-add-links)
         ("C-c N b" . denote-link-backlinks)
         ("C-c N f" . denote-link-find-file)))

(provide 'lang-org)
;;; lang-org.el ends here
