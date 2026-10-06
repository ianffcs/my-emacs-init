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

WhatsAppel's bridge token uses the same resolver. Add an entry with
`host=127.0.0.1` and `user=whatsappel`, or put this in `~/.authinfo.gpg`:

```text
machine 127.0.0.1 login whatsappel password <WHATSAPPEL_TOKEN>
```

The configured WhatsAppel client remains lazy-loaded, and its bridge must be
installed and running separately before `C-c T w` can connect.

## AI provider table

AI providers are defined once in `ian/ai-providers` (`core-auth.el`), keyed by
provider symbol. Each row gives the auth-source `:host`, optional `:aliases`
and the `:env` fallback variable:

```elisp
(anthropic :host "api.anthropic.com" :aliases ("api.claude.ai")
           :env "ANTHROPIC_API_KEY")
```

Rows exist for `openai`, `anthropic` and `google`. Code asks for keys by
symbol: `(ian/ai-key 'anthropic)`, or `(ian/ai-key 'anthropic t)` to get nil
instead of an error when missing. `(ian/ai-key-callback 'openai)` returns a
closure for packages that take a key function. An unknown provider symbol
always signals an error. To add a provider, add a row to the table.

## Local service hosts

Hosts and ports of local services are resolved the same way, with the value
in the password field. `ian/ollama-host` (in `core-auth.el`) looks up `user=host`, so gptel,
ellama, and Minuet all follow one entry:

```text
machine ollama login host password <HOST:PORT>
```

For example `machine ollama login host password 10.100.0.2:11434`. The
`IAN_OLLAMA_HOST` environment variable is used as a fallback when no
auth-source entry exists. No IP addresses are hardcoded in the modules.

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
