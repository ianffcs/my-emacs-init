;;; org-pessoal.el --- Org-mode pessoal -*- lexical-binding: t; -*-
(setq ian/org-master-file
      (expand-file-name "~/org/planos.org"))

(setq ian/org-project-files
      '(("~/org/vida.org")
        ("~/org/infra.org")
        ("~/org/ia.org")
        ("~/org/tecnica.org")
        ("~/org/trabalho.org")))

(setq org-agenda-files (mapcar #'car ian/org-project-files))


(setq org-capture-templates
      `(
        ;; ── Ideia geral (fica no topo, antes de tudo)
        ("i" "Ideia geral"
         entry (file+headline ,ian/org-master-file "Vida")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Ideia\n  :END:\n"
         :empty-lines 1)

        ;; ── Tarefa de Saúde & Bem‑estar
        ("s" "Saúde & Bem‑estar"
         entry (file+headline ,ian/org-master-file "Saúde & Bem‑estar")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Saúde\n  :END:\n"
         :empty-lines 1)

        ;; ── Tarefa de Finanças Pessoais
        ("f" "Finanças Pessoais"
         entry (file+headline ,ian/org-master-file "Finanças Pessoais")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Finanças\n  :END:\n"
         :empty-lines 1)

        ;; ── Tarefa de Moradia
        ("m" "Moradia"
         entry (file+headline ,ian/org-master-file "Moradia")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Moradia\n  :END:\n"
         :empty-lines 1)

        ;; ── Tarefa de Infraestrutura / Aplicações (sub‑título genérico)
        ("a" "Infraestrutura / Aplicações"
         entry (file+headline ,ian/org-master-file "Infraestrutura / Aplicações")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Infra\n  :END:\n"
         :empty-lines 1)

        ;; ── Ideia para Knowledge‑Graph (exemplo de sub‑seção)
        ("k" "Knowledge‑Graph Project"
         entry (file+headline ,ian/org-master-file "Knowledge‑Graph Project")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: KG\n  :END:\n"
         :empty-lines 1)

        ;; ── Ideia para Modelos & IA (ex.: nova quantização)
        ("g" "Modelos & IA"
         entry (file+headline ,ian/org-master-file "Modelos & Quantizações")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: IA\n  :END:\n"
         :empty-lines 1)

        ;; ── Ideia de Física & Matemática
        ("p" "Física & Matemática"
         entry (file+headline ,ian/org-master-file "Física & Matemática")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Física\n  :END:\n"
         :empty-lines 1)

        ;; ── Ideia de Trabalho / Depuração
        ("w" "Trabalho"
         entry (file+headline ,ian/org-master-file "Depuração")
         "* TODO %?\n  %U\n  :PROPERTIES:\n  :CATEGORY: Trabalho\n  :END:\n"
         :empty-lines 1)))

;; Usa a tecla `C-c c` (padrão) para abrir o menu de captura.
;;(global-set-key (kbd "C-c c") 'org-capture)

;; Opcional: atalhos diretos para cada template
;; (global-set-key
;;  (kbd "C-c c i"
;;       (lambda ()
;;         (interactive)
;;         (org-capture nil "i"))))
;; (global-set-key
;;  (kbd "C-c c s")
;;  (lambda ()
;;    (interactive)
;;    (org-capture nil "s")))
;; (global-set-key
;;  (kbd "C-c c f")
;;  (lambda ()
;;    (interactive)
;;    (org-capture nil "f")))
;;(global-set-key (kbd "C-c r") 'org-refile)

(setq org-refile-targets
      '(("planos.org" :maxlevel . 4)))  ;; permite refazer até 4 níveis de profundidade

(setq org-agenda-custom-commands
      '(("d" "Deadlines próximos (próximos 7 dias)"
         agenda ""
         ((org-agenda-span 7)
          (org-agenda-start-day "+0d")
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'todo 'done 'scheduled 'timestamp)) ; só DEADLINES
          (org-agenda-overriding-header "⚡ Deadlines próximos")))
        ("p" "Projetos em foco – só TODO + :focus:"
         alltodo ""
         ((org-agenda-files (list "~/org/infra.org" "~/org/ia.org"))
          (org-agenda-tag-filter-preset '("-someday" "-blocked"))
          (org-agenda-overriding-header "🚀 Projetos em foco")
          (org-super-agenda-groups
           '((:name "Urgente"
                    :todo "NEXT"
                    :order 1)
             (:name "Hoje"
                    :deadline today
                    :order 2)
             (:name "Esta semana"
                    :deadline future
                    :order 3)))))
        ("s" "Someday/Maybe – tudo que está em WAIT ou com a tag :someday:"
         tags-todo "+someday"
         ((org-agenda-overriding-header "🌙 Someday/Maybe")
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'todo 'done))))))

;; Atalho rápido para arquivar a sub‑árvore corrente
;; (global-set-key (kbd "C-c C-x a") 'org-archive-subtree-default)

;; ;; Onde o Org coloca o conteúdo arquivado
;; (setq org-archive-location "~/org/archive.org::datetree/")
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)"
;;                   "NEXT(n)"
;;                   "WAIT(w@/!)"
;;                   "SOMEDAY(s)"
;;                   "|"
;;                   "DONE(d)"
;;                   "CANCELLED(c)")))
;; (setq org-agenda-files (list "~/org/infra.org" "~/org/ia.org")) ;; só os projetos que importam

;; (use-package org-super-agenda
;;   :ensure t
;;   :config
;;   (org-super-agenda-mode))

;; (setq org-agenda-custom-commands
;;       '(("z" "Minha agenda super‑agregada"
;;          ((agenda ""                                   ; calendário normal
;;                   (org-super-agenda-groups
;;                    '((:name "Hoje"
;;                             :time-grid t
;;                             :date today)
;;                      (:name "Urgente"
;;                             :priority "A")
;;                      (:name "Saúde"
;;                             :tag "Saúde")
;;                      (:name "Finanças"
;;                             :tag "Finanças")
;;                      (:name "Projetos"
;;                             :tag "Projeto")
;;                      (:name "Outros"
;;                             :anything t))))))))

(provide 'org-pessoal)
;;; org-pessoal.el ends here
