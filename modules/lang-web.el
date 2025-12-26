;;; lang-web.el --- Web Development (HTML, CSS, JS, TS) -*- lexical-binding: t; -*-

;;; Commentary:
;; Web development: HTML, CSS, JavaScript, TypeScript, JSON, YAML.
;; Tree-sitter modes are preferred where available.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. HTML / TEMPLATES
;; ============================================================================

;; Prefer html-ts-mode for plain HTML.
(use-package html-ts-mode
  :straight (:type built-in)
  :mode (("\\.html?\\'" . html-ts-mode))
  :hook (html-ts-mode . (lambda ()
                          (setq-local sgml-basic-offset 2))))

;; Use web-mode for templating / component formats.
(use-package web-mode
  :mode (("\.erb\'"        . web-mode)
         ("\.ejs\'"        . web-mode)
         ("\.hbs\'"        . web-mode)
         ("\.mustache\'"   . web-mode)
         ("\.djhtml\'"     . web-mode)
         ("\.jinja2?\'"    . web-mode)
         ("\.twig\'"       . web-mode)
         ("\.vue\'"        . web-mode)
         ("\.svelte\'"     . web-mode)
         ("\.blade\.php\'" . web-mode)
         ("\.phtml\'"      . web-mode)
         ("\.tpl\.php\'"  . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  (web-mode-block-padding 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t))

;; ============================================================================
;; 2. CSS / SCSS
;; ============================================================================

(use-package css-ts-mode
  :straight (:type built-in)
  :mode (("\\.css\\'" . css-ts-mode))
  :hook (css-ts-mode . (lambda ()
                         (setq-local tab-width 2)
                         (setq-local indent-tabs-mode nil))))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

;; ============================================================================
;; 3. EMMET
;; ============================================================================

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-ts-mode . emmet-mode)
         (css-ts-mode . emmet-mode)
         (scss-mode . emmet-mode)
         (sgml-mode . emmet-mode))
  :custom
  (emmet-move-cursor-between-quotes t)
  (emmet-self-closing-tag-style " /"))

;; ============================================================================
;; 4. JAVASCRIPT (TREE-SITTER PREFERRED)
;; ============================================================================

(use-package js-ts-mode
  :straight (:type built-in)
  :mode (("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode))
  :hook (js-ts-mode . (lambda ()
                        (setq-local js-indent-level 2))))

;; JSX
(use-package js-ts-mode
  :straight (:type built-in)
  :mode (("\\.jsx\\'" . js-ts-mode)))

;; ============================================================================
;; 5. TYPESCRIPT / TSX
;; ============================================================================

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :hook (typescript-ts-mode . (lambda ()
                                (setq-local typescript-indent-level 2))))

(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :hook (tsx-ts-mode . (lambda ()
                         (setq-local typescript-indent-level 2))))

;; ============================================================================
;; 6. JSON (TREE-SITTER PREFERRED)
;; ============================================================================

(use-package json-ts-mode
  :straight (:type built-in)
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.jsonc\\'" . json-ts-mode)
         ("\\.prettierrc\\'" . json-ts-mode)
         ("composer\\.lock\\'" . json-ts-mode))
  :hook (json-ts-mode . (lambda ()
                          (setq-local js-indent-level 2))))

(use-package json-reformat
  :commands (json-reformat-region json-reformat-buffer)
  :custom
  (json-reformat:indent-width 2)
  (json-reformat:pretty-string? t))

;; ============================================================================
;; 7. YAML (TREE-SITTER PREFERRED)
;; ============================================================================

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode (("\\.ya?ml\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . (lambda ()
                          (setq-local tab-width 2)
                          (setq-local indent-tabs-mode nil))))

;; Keep yaml-mode for odd cases (templated yaml, etc.).
(use-package yaml-mode
  :mode (("\\.yml\\.j2\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
                       (setq-local tab-width 2)
                       (setq-local indent-tabs-mode nil))))

;; ============================================================================
;; 8. NODE.JS / NPM
;; ============================================================================

(use-package nodejs-repl
  :commands nodejs-repl
  :bind (:map js-ts-mode-map
              ("C-c C-z" . nodejs-repl)
              ("C-c C-e" . nodejs-repl-send-last-expression)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-b" . nodejs-repl-send-buffer)))

(use-package npm-mode
  :hook ((js-ts-mode . npm-mode)
         (typescript-ts-mode . npm-mode)
         (tsx-ts-mode . npm-mode)))

;; ============================================================================
;; 9. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; JS/TS/TSX
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) '(prettier))
  ;; HTML/Templates
  (setf (alist-get 'html-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'web-mode apheleia-mode-alist) '(prettier))
  ;; JSON/YAML
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'yaml-mode apheleia-mode-alist) '(prettier))
  ;; CSS
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'scss-mode apheleia-mode-alist) '(prettier)))

(provide 'lang-web)
;;; lang-web.el ends here
