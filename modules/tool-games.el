;;; tool-games.el --- Games and entertainment -*- lexical-binding: t; -*-

;;; Commentary:
;; Fun tools and game integrations.

;;; Code:

(use-package nethack
  :straight (nethack :type git :host github :repo "Feyorsh/nethack-el")
  :custom
  (nethack-program "/Users/ianffcs/src/nethack-el/build/games/nethack")
  :commands (nethack nethack-remote nethack-lisprec-playback))

(provide 'tool-games)
;;; tool-games.el ends here
