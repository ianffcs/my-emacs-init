---
name: emacs-plstore-gpg-key-mismatch
description: Delete stale oauth2.plstore when GPG decryption fails.
category: emacs
---

When Gnus tries to connect to Outlook via OAuth2, it fails with "No secret key" — **diagnose first, because the message is misleading**. It can mean either the key is missing from your keyring, OR gpg-agent/pinentry can't access it. Deleting the file loses tokens unless the key is truly gone.

**Diagnose:** Run:
```bash
gpg --list-secret-keys <KEY-ID-HERE>
```

- **If the key appears:** The key is in your keyring but gpg-agent can't access it (stale agent, missing pinentry, no cached passphrase, or GPG_TTY issue). Run `gpgconf --kill gpg-agent`, then retry `M-x gnus`. Test with `echo hi | gpg -e -r <KEY-ID> | gpg -d` to confirm a passphrase prompt works.
- **If the key does NOT appear:** The key is truly missing. Delete the file and re-authenticate:
  ```bash
  rm ~/.emacs.d/oauth2.plstore
  ```
  Gnus will recreate it on next connection (`M-x gnus`), prompting for OAuth2 re-authentication.

**Optional:** Add a startup hook to auto-recreate the file if missing, so `M-x gnus` silently restores it with zero downtime. In `modules/tool-comm.el` (inside the gnus `:config`):
```elisp
(defun ian/ensure-oauth2-plstore ()
  "Initialize oauth2.plstore if missing by opening the plstore connection."
  (let ((plstore-file (expand-file-name "oauth2.plstore" user-emacs-directory)))
    (unless (file-exists-p plstore-file)
      (require 'plstore)
      (let ((store (plstore-open plstore-file)))
        (plstore-close store)))))
(add-hook 'gnus-startup-hook #'ian/ensure-oauth2-plstore)
```