;;; lang-latex.el --- LaTeX & Document Viewing -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for LaTeX and document viewing:
;; - AUCTeX for LaTeX editing
;; - RefTeX for cross-references
;; - PDF-tools for viewing
;; - EPUB and DjVu support
;; - BibTeX management
;;
;; Split from lang-text.el for better organization.

;;; Code:

;; ============================================================================
;; 1. LATEX & AUCTEX
;; ============================================================================

(use-package tex
  :straight auctex
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.ltx\\'" . latex-mode)
         ("\\.cls\\'" . latex-mode)
         ("\\.sty\\'" . latex-mode)
         ("\\.bbl\\'" . latex-mode))
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . TeX-source-correlate-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-PDF-mode t)
  (TeX-engine 'xetex)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (setq-default TeX-master nil))

;; ============================================================================
;; 2. REFTEX (Cross-references)
;; ============================================================================

(use-package reftex
  :straight (:type built-in)
  :after tex
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography '("~/Documents/bibliography.bib")))

;; ============================================================================
;; 3. COMPANY COMPLETION FOR LATEX
;; ============================================================================

(use-package company-auctex
  :after (company tex)
  :config
  (company-auctex-init))

;; Math preview in buffer
(use-package company-math
  :after (company tex))

;; ============================================================================
;; 4. PDF VIEWING
;; ============================================================================

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-tools-enable-minor-modes)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install :no-query))

;; ============================================================================
;; 5. EPUB VIEWING
;; ============================================================================

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . visual-line-mode)
  :custom
  (nov-text-width t))

;; ============================================================================
;; 6. DJVU VIEWING
;; ============================================================================

(use-package djvu
  :mode ("\\.djvu\\'" . djvu-read-mode))

;; ============================================================================
;; 7. ORG-NOTER (Annotate PDFs with Org)
;; ============================================================================

(use-package org-noter
  :after (org pdf-tools)
  :custom
  (org-noter-notes-search-path (list (expand-file-name "notes" org-directory))))

;; ============================================================================
;; 8. BIBTEX
;; ============================================================================

(use-package bibtex
  :straight (:type built-in)
  :mode ("\\.bib\\'" . bibtex-mode)
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-"))

;; ============================================================================
;; 9. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; LaTeX (latexindent)
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "-"))
  (setf (alist-get 'latex-mode apheleia-mode-alist) '(latexindent)))

(provide 'lang-latex)
;;; lang-latex.el ends here
