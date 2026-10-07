  ;; Recreate oauth2.plstore if missing (after GPG key rotation).
  (defun ian/ensure-oauth2-plstore ()
    "Initialize oauth2.plstore if missing by opening the plstore connection."
    (let ((plstore-file (expand-file-name "oauth2.plstore" user-emacs-directory)))
      (unless (file-exists-p plstore-file)
        (require 'plstore)
        (let ((store (plstore-open plstore-file)))
          (plstore-close store)))))

  (add-hook 'gnus-startup-hook #'ian/ensure-oauth2-plstore)