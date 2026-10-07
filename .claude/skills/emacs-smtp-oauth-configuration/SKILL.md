---
name: emacs-smtp-oauth-configuration
category: emacs
description: Use when configuring smtpmail to use OAuth via auth-source.
---

Set `smtpmail-smtp-user` to your email address so the xoauth2-plugin can match the right `.authinfo.gpg` entry. Set `smtpmail-auth-supported` to `'(xoauth2)` only to prevent fallback to password auth.

## SMTP configuration

```elisp
(use-package smtpmail
  :custom
  (message-send-mail-function #'smtpmail-send-it)
  (smtpmail-smtp-server "smtp.office365.com")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (smtpmail-smtp-user "your-email@outlook.com")
  (smtpmail-auth-supported '(xoauth2)))
```

## .authinfo.gpg entry

```
machine smtp.office365.com login user@outlook.com port 587 auth xoauth2 auth-source-xoauth2-predefined-service microsoft
```

This aligns SMTP with IMAP (which uses `nnimap-authenticator xoauth2`), so both clients share token refresh and neither needs stored passwords.