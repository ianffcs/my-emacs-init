(require 'package)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(org-default-notes-file (concat org-directory "/life.org"))
 '(org-directory "~/sync/orgfiles")
 '(org-export-html-postamble nil)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 4.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-hide-leading-stars t)
 '(org-journal-dir "~/sync/orgfiles")
 '(org-reveal-plugins (quote (classList markdown highlight zoom notes)))
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (flycheck-rust dired-avfs fireplace elnode web-server cmake-mode makefile-executor ledger-mode magit-gitflow discover-clj-refactor clj-refactor disk-usage writeroom-mode flycheck-plantuml plantuml-mode sml-modeline ob-sml emoji-display emojify-logos lisp-extra-font-lock wakatime-mode ivy-bibtex ivy-explorer ivy-historian ivy-hydra ivy-mpdel ivy-pages ivy-pass ivy-yasnippet ix jq-mode json-mode json-navigator json-reformat json-rpc json-snatcher jvm-mode kanban kdeconnect prettify-symbols-mode org-sql treemacs-magit treemacs-icons-dired 2048-game virtualenvwrapper org-bullets meghanada suggest drag-stuff visual-regexp-steroids visual-regexp direx dired-k dired-hacks-utils org-web-tools emmet-mode org-download org-ref org-reveal org-reval rjsx-mode jsx-mode indium csv-mode org-babel-eval-in-repl scad-preview scad-mode ob-clojure-literate htmlize ox-reveal neotree neon-mode closql weather-metno all-the-icons-gnus all-the-icons-ivy all-the-icons-dired company-tern ess-view ess-R-data-view org-pomodoro ob-sql-mode ob-clojurescript org-journal selectric-mode pretty-symbols markdown-mode pandoc-mode quelpa-use-package quelpa ob-ipython pipenv clojure-mode-extra-font-locking emms-player-mpv-jp-radios emms-info-mediainfo slack gnugo yaml-mode flymake-yaml flx nlinum-relative ac-geiser sly-repl-ansi-color sly-quicklisp ac-sly sly geiser counsel-spotify spotify multi-term haskell-emacs flycheck-clojure haskell-mode ac-cider ibuffer-tramp pretty-mode better-defaults ein py-autopep8 linum-relative vimrc-mode cyberpunk-theme auto-auto-indent color-theme try which-key use-package solarized-theme rainbow-delimiters python-mode parinfer org-link-minor-mode nyan-mode evil-smartparens evil-org evil-magit evil-cleverparens edn auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "DarkGoldenrod2")))))
