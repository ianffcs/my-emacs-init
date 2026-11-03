;;; ui-icons-test.el --- Tests for frame-aware icon display -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q -l ert -l test/ui-icons-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'seq)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'core-os t)
(test-helper-load-module 'core-ui t)

(defmacro ui-icons-test--with (graphic-frames env &rest body)
  "Run BODY with frames GRAPHIC-FRAMES graphical and IAN_EMACS_TTY_ICONS=ENV."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'display-graphic-p)
              (lambda (&optional f) (and (memq f ,graphic-frames) t)))
             ((symbol-function 'selected-frame) (lambda () 'tty))
             ((symbol-function 'frame-list) (lambda () '(tty gui))))
     (let ((process-environment
            (cons (if ,env (concat "IAN_EMACS_TTY_ICONS=" ,env) "IAN_EMACS_TTY_ICONS")
                  process-environment)))
       ,@body)))

(ert-deftest icons-displayable-follows-frame ()
  (ui-icons-test--with '(gui) nil
    (should (ian/icons-displayable-p 'gui))
    (should-not (ian/icons-displayable-p 'tty))
    (should-not (ian/icons-displayable-p))))

(ert-deftest icons-displayable-env-override ()
  (ui-icons-test--with nil "1"
    (should (ian/icons-displayable-p 'tty))
    (should (ian/icons-displayable-p))))

(ert-deftest apply-icon-settings-per-frame ()
  (ui-icons-test--with '(gui) nil
    (let ((ian/icon-variables '(ui-icons-test--a ui-icons-test--b)))
      (ian/apply-icon-settings 'gui)
      (should (eq t ui-icons-test--a))
      (should (eq t ui-icons-test--b)))))

(ert-deftest apply-icon-settings-tty-keeps-icons-with-graphic-frame ()
  (ui-icons-test--with '(gui) nil
    (let ((ian/icon-variables '(ui-icons-test--a)))
      (ian/apply-icon-settings 'tty)
      (should (eq t ui-icons-test--a)))))

(ert-deftest apply-icon-settings-tty-only-turns-off ()
  (ui-icons-test--with nil nil
    (let ((ian/icon-variables '(ui-icons-test--a)))
      (ian/apply-icon-settings 'tty)
      (should (null ui-icons-test--a)))))

(provide 'ui-icons-test)
;;; ui-icons-test.el ends here
