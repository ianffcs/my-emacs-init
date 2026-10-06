---
name: emacs-named-path-resolver
category: emacs
description: Use when centralizing named file definitions across modules.
---

Define a central table of named files (e.g., `ian/org-files`) in the semantic owner module (typically `core-settings`); provide a resolver function (e.g., `ian/org-file`) that returns the absolute path for a named key, resolved at call time. Update all consumers to call the resolver instead of hardcoding or duplicating the path computation.

This consolidation eliminates duplication, makes paths easy to audit and change in one place, and allows tests to verify no module outside the owner hardcodes the file names.