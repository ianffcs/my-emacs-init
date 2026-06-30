;;; lang-carp.el --- Carp language support -*- lexical-binding: t; -*-

;;; Commentary:
;; Carp mode, REPL integration, and Flycheck support.

;;; Code:

(use-package carp-mode
  :straight (:host github :repo "carp-lang/carp-emacs"
             :files ("*.el"))
  :after clojure-mode
  :mode ("\\.carp\\'" . carp-mode)
  :hook ((carp-mode . subword-mode)
         (carp-mode . parinfer-rust-mode))
  :config
  (require 'inf-carp-mode)
  (when (require 'carp-flycheck nil t)
    (add-hook 'carp-mode-hook #'flycheck-mode)))

(provide 'lang-carp)
;;; lang-carp.el ends here

