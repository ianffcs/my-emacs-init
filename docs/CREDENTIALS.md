# Credentials

`core-auth.el` resolves credentials through Emacs `auth-source`. The lookup
order is `~/.authinfo.gpg`, `~/.netrc`, then the Secret Service `Login`
collection. API keys fall back to the host-specific environment variable when
no non-empty auth-source secret is found.

## KeePassXC

Emacs uses the Secret Service API, so KeePassXC's CLI is not invoked and the
database password is never passed to a subprocess. In KeePassXC, enable **Tools
→ Settings → Secret Service Integration**, unlock the database, then expose the
group containing the credential entries in that database's Secret Service
settings. Only one Secret Service provider can be active at a time.

For entries used by Emacs, add unprotected custom attributes named `host` and
`user`, matching the values requested by the application. Keep the credential
in KeePassXC's protected **Password** field. For example, an API entry uses
`host=api.openai.com` and `user=apikey`; a NickServ entry uses its IRC server
as `host` and `user=your-nick`. The `Login` collection is queried after the
existing authinfo and netrc files, so those files keep precedence when the
same host and user are present in more than one source.

To check KeePassXC access without displaying a credential, evaluate this with
`M-:` after substituting the desired host and user:

```elisp
(and (auth-source-search :host "api.openai.com"
                         :user "apikey"
                         :require '(:secret))
     t)
```

It returns only `t` or `nil`; do not print or evaluate the result's `:secret`
when checking the setup.
