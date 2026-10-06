---
name: emacs-credential-provider-table
category: emacs
description: Use when consolidating provider credentials across modules.
---

Instead of separate alists keyed by host, service, and provider names, define one table (e.g., `ian/ai-providers`) keyed by provider symbol (`'openai`, `'anthropic`, etc.), with each row holding `:host`, `:aliases` (optional), and `:env-var`. Provide one resolver function (e.g., `ian/ai-key`) that takes a provider symbol and returns the key via auth-source (searching host then aliases with user `"apikey"`) or env-var fallback.

This consolidation reduces duplication across modules and creates a single source of truth for provider metadata.