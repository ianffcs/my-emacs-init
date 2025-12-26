;;; lang-ops.el --- DevOps Tools (Terraform, Ansible, K8s) -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for DevOps: Terraform, Ansible, Kubernetes, Docker, etc.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. TERRAFORM
;; ============================================================================

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode)
  :hook ((terraform-mode . terraform-format-on-save-mode)
         (terraform-mode . subword-mode))
  :custom
  (terraform-indent-level 2))

;; Terraform documentation
(use-package terraform-doc
  :after terraform-mode
  :commands terraform-doc)

;; ============================================================================
;; 2. HCL
;; ============================================================================

(use-package hcl-mode
  :mode (("\\.hcl\\'"     . hcl-mode)
         ("\\.nomad\\'"   . hcl-mode)
         ("\\.tf\\.json\\'" . hcl-mode)))

;; ============================================================================
;; 3. ANSIBLE
;; ============================================================================

;; NOTE: the package provides minor-mode `ansible` (and `ansible-doc` etc.)
(use-package ansible
  :hook ((yaml-mode yaml-ts-mode) . (lambda ()
                                      (when (and buffer-file-name
                                                 (or (string-match-p "ansible" buffer-file-name)
                                                     (string-match-p "playbook" buffer-file-name)
                                                     (string-match-p "roles" buffer-file-name)))
                                        (ansible 1)))))

(use-package ansible-doc
  :after ansible
  :commands ansible-doc)

(use-package company-ansible
  :after (company ansible)
  :config
  (add-to-list 'company-backends 'company-ansible))

;; ============================================================================
;; 4. DOCKER
;; ============================================================================

(use-package dockerfile-mode
  :mode ("\\.dockerfile\\'" . dockerfile-mode))

(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerfile\\'" . dockerfile-ts-mode)))

(use-package docker-compose-mode
  :mode (("docker-compose\\.ya?ml\\'" . docker-compose-mode)
         ("compose\\.ya?ml\\'" . docker-compose-mode)))

;; Docker management
(use-package docker
  :bind (("C-c D" . docker))
  :custom
  (docker-run-as-root t))

;; ============================================================================
;; 5. KUBERNETES
;; ============================================================================

;; Kubernetes config files
(use-package k8s-mode
  :mode ("\\.k8s\\'" . k8s-mode))

;; Kubernetes management
(use-package kubel
  :commands kubel
  :bind (("C-c K" . kubel))
  :config
  (kubel-vterm-setup))

;; Kubernetes Helm templates in YAML
(use-package kubernetes-helm
  :after (yaml-mode yaml-ts-mode)
  :hook ((yaml-mode yaml-ts-mode) . (lambda ()
                                      (when (and buffer-file-name
                                                 (string-match-p "templates" buffer-file-name))
                                        (kubernetes-helm-mode 1)))))

;; ============================================================================
;; 6. NIX
;; ============================================================================

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :hook (nix-mode . subword-mode))

(use-package nix-ts-mode
  :straight (:type built-in)
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook (nix-ts-mode . subword-mode))

;; ============================================================================
;; 7. VAGRANT
;; ============================================================================

(use-package vagrant
  :commands (vagrant-up vagrant-ssh vagrant-halt))

(use-package vagrant-tramp
  :after tramp
  :config
  (with-eval-after-load 'tramp
    (vagrant-tramp-enable)))

;; ============================================================================
;; 8. SYSTEMD
;; ============================================================================

(use-package systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.timer\\'"   . systemd-mode)
         ("\\.socket\\'"  . systemd-mode)
         ("\\.target\\'"  . systemd-mode)
         ("\\.mount\\'"   . systemd-mode)
         ("\\.path\\'"    . systemd-mode)))

;; ============================================================================
;; 9. NGINX
;; ============================================================================

(use-package nginx-mode
  :mode (("nginx.*\\.conf\\'" . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode)))

(use-package company-nginx
  :after (company nginx-mode)
  :hook (nginx-mode . (lambda ()
                        (add-to-list 'company-backends 'company-nginx))))

;; ============================================================================
;; 10. APACHE
;; ============================================================================

(use-package apache-mode
  :mode (("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("apache2?\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

;; ============================================================================
;; 11. SSH CONFIG
;; ============================================================================

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'"  . ssh-config-mode)
         ("/known_hosts\\'"   . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

;; ============================================================================
;; 12. PKGBUILD (Arch Linux)
;; ============================================================================

(use-package pkgbuild-mode
  :mode ("PKGBUILD\\'" . pkgbuild-mode))

;; ============================================================================
;; 13. CLOUDFORMATION
;; ============================================================================

(use-package cfn-mode
  :straight (:host gitlab :repo "worr/cfn-mode")
  :mode (("\\.template\\'" . cfn-mode)
         ("\\.cfn\\.json\\'" . cfn-mode)
         ("\\.cfn\\.yaml\\'" . cfn-mode)
         ("-template\\.json\\'" . cfn-mode)
         ("-template\\.yaml\\'" . cfn-mode)))

;; CloudFormation YAML files - detect by content
(defun ian/maybe-cfn-mode ()
  "Enable cfn-mode if file looks like CloudFormation."
  (when (and buffer-file-name
             (string-match-p "\\.ya?ml\\'" buffer-file-name)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "AWSTemplateFormatVersion\\|Resources:\\|Type: AWS::" 1000 t)))
    (cfn-mode)))

(add-hook 'yaml-mode-hook #'ian/maybe-cfn-mode)
(add-hook 'yaml-ts-mode-hook #'ian/maybe-cfn-mode)

;; SAM (Serverless Application Model) templates
(add-to-list 'auto-mode-alist '("template\\.yaml\\'" . cfn-mode))
(add-to-list 'auto-mode-alist '("samconfig\\.toml\\'" . toml-mode))

;; AWS CLI helpers
(defun ian/cfn-validate ()
  "Validate CloudFormation template using AWS CLI."
  (interactive)
  (let ((file (buffer-file-name)))
    (compile (format "aws cloudformation validate-template --template-body file://%s" file))))

(defun ian/cfn-deploy (stack-name)
  "Deploy CloudFormation stack with STACK-NAME."
  (interactive "sStack name: ")
  (let ((file (buffer-file-name)))
    (compile (format "aws cloudformation deploy --template-file %s --stack-name %s --capabilities CAPABILITY_IAM CAPABILITY_NAMED_IAM"
                     file stack-name))))

;; ============================================================================
;; 14. LSP CONFIGURATION (EGLOT)
;; ============================================================================

(with-eval-after-load 'eglot
  ;; Terraform
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve")))

  ;; Dockerfile
  (add-to-list 'eglot-server-programs
               '((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio")))

  ;; Nix
  (add-to-list 'eglot-server-programs
               '((nix-mode nix-ts-mode) . ("nil"))))

;; NOTE: Ansible LSP should be configured per-project via .dir-locals.el:
;; ((yaml-mode . ((eglot-server-programs . ((yaml-mode . ("ansible-language-server" "--stdio")))))))


;; ============================================================================
;; 15. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Terraform
  (setf (alist-get 'terraform apheleia-formatters)
        '("terraform" "fmt" "-"))
  (setf (alist-get 'terraform-mode apheleia-mode-alist)
        '(terraform))

  ;; Nix
  (setf (alist-get 'nixfmt apheleia-formatters)
        '("nixfmt"))
  (setf (alist-get 'alejandra apheleia-formatters)
        '("alejandra" "-"))
  (setf (alist-get 'nix-mode apheleia-mode-alist)
        '(alejandra)))

;; ============================================================================
;; 16. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/ops-menu ()
                           "DevOps commands"
                           ["Docker"
                            ("d" "Docker" docker)]
                           ["CloudFormation"
                            ("c v" "Validate" ian/cfn-validate)
                            ("c d" "Deploy" ian/cfn-deploy)]
                           ["Kubernetes"
                            ("k" "Kubel" kubel)]
                           ["Terraform"
                            ("t d" "Doc" terraform-doc)]
                           ["Vagrant"
                            ("v u" "Up" vagrant-up)
                            ("v s" "SSH" vagrant-ssh)
                            ("v h" "Halt" vagrant-halt)])
  (global-set-key (kbd "C-c O") #'ian/ops-menu))

(provide 'lang-ops)
;;; lang-ops.el ends here
