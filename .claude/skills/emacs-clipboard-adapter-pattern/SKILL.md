---
name: emacs-clipboard-adapter-pattern
category: emacs
description: Use when copy/paste dispatch per-frame to different tools.
---

Define an adapter selector that evaluates at **call time** and returns a symbol (`wsl`, `gui`, `wayland`, `x11`, nil) identifying which tool is available for the current frame. Route interprogram-cut-function and interprogram-paste-function through a dispatcher that calls the selector and invokes the right adapter's logic.

Evaluate the selector *inside* the command, not at load time. This allows behavior to follow the current frame's actual capabilities, fixing frame-mismatch bugs where load-time configuration doesn't adapt to new frames.