;;; ui-theme-test.el --- Tests for the theme switching logic -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads modules/core-ui.el with `use-package' stubbed out and exercises
;; the hour-based theme decisions directly.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/ui-theme-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'core-ui t)

(defun ui-theme-test--capture (hour graphic-p)
  "Return the theme `ian/auto-theme' picks at HOUR on a GRAPHIC-P frame."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) graphic-p))
            ((symbol-function 'format-time-string) (lambda (&rest _) hour))
            ((symbol-function 'ian/activate-theme) (lambda (theme) (throw 'theme theme))))
    (catch 'theme (ian/auto-theme))))

(ert-deftest auto-theme-graphic-frame-picks-by-hour ()
  (should (eq ian/light-theme (ui-theme-test--capture "10" t)))
  (should (eq ian/dark-theme (ui-theme-test--capture "23" t)))
  (should (eq ian/dark-theme (ui-theme-test--capture "06" t)))
  (should (eq ian/light-theme (ui-theme-test--capture "18" t))))

(ert-deftest auto-theme-terminal-is-always-dark ()
  (should (eq ian/dark-theme (ui-theme-test--capture "10" nil))))

(ert-deftest toggle-theme-alternates ()
  (cl-letf (((symbol-function 'ian/activate-theme) #'identity))
    (let ((custom-enabled-themes (list ian/dark-theme)))
      (should (eq ian/light-theme (ian/toggle-theme))))
    (let ((custom-enabled-themes (list ian/light-theme)))
      (should (eq ian/dark-theme (ian/toggle-theme))))))

(provide 'ui-theme-test)
;;; ui-theme-test.el ends here
