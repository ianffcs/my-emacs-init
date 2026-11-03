;;; platform-test.el --- Tests for the platform interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises `ian/platform' dispatchers in modules/core-os.el by injecting a
;; fake platform through `ian/platform-override'.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/platform-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'core-os t)

(defmacro platform-test--on (platform &rest body)
  "Run BODY as if on PLATFORM."
  (declare (indent 1))
  `(let ((ian/platform-override ,platform)) ,@body))

(ert-deftest platform-override-wins ()
  (dolist (p '(wsl darwin windows linux))
    (platform-test--on p (should (eq p (ian/platform))))))

(ert-deftest platform-wsl-p-follows-platform ()
  (platform-test--on 'wsl (should (ian/wsl-p)))
  (platform-test--on 'linux (should-not (ian/wsl-p)))
  (platform-test--on 'darwin (should-not (ian/wsl-p))))

(ert-deftest platform-detect-is-one-of-known ()
  (let ((ian/platform-override nil))
    (should (memq (ian/platform) '(wsl darwin windows linux)))))

(ert-deftest platform-wsl-probe-is-memoized ()
  (let ((ian/platform-override nil) (ian/wsl-p-cache nil) (system-type 'gnu/linux) (n 0))
    (cl-letf (((symbol-function 'ian/platform--detect-wsl) (lambda () (cl-incf n) t)))
      (should (eq 'wsl (ian/platform)))
      (should (eq 'wsl (ian/platform)))
      (should (= n 1)))))

(ert-deftest platform-volume-command-per-platform ()
  (should (equal '("amixer" "set" "Master" "2%+")
                 (platform-test--on 'linux (ian/platform-volume-command 'up))))
  (should (equal '("amixer" "set" "Master" "2%-")
                 (platform-test--on 'wsl (ian/platform-volume-command 'down))))
  (should (equal '("amixer" "set" "Master" "toggle")
                 (platform-test--on 'linux (ian/platform-volume-command 'toggle))))
  (dolist (a '(up down toggle))
    (let ((cmd (platform-test--on 'darwin (ian/platform-volume-command a))))
      (should (equal '("osascript" "-e") (cl-subseq cmd 0 2))))
    (should-not (platform-test--on 'windows (ian/platform-volume-command a)))))

(ert-deftest platform-open-external-wsl-uses-wslview ()
  (let (got)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_n _b &rest args) (setq got args)))
              ((symbol-function 'w32-shell-execute) (lambda (&rest _) (error "wrong"))))
      (platform-test--on 'wsl (ian/open-external "/tmp/a.pdf")))
    (should (equal got '("wslview" "/tmp/a.pdf")))))

(ert-deftest platform-open-external-other-platforms ()
  (let (got shell)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_n _b &rest args) (setq got args)))
              ((symbol-function 'w32-shell-execute) (lambda (&rest args) (setq shell args))))
      (platform-test--on 'linux (ian/open-external "f"))
      (should (equal got '("xdg-open" "f")))
      (platform-test--on 'darwin (ian/open-external "f"))
      (should (equal got '("open" "f")))
      (setq got nil)
      (platform-test--on 'windows (ian/open-external "f"))
      (should-not got)
      (should (equal shell '("open" "f"))))))

(ert-deftest platform-screenshot-command ()
  (should (equal "screencapture -i %s" (platform-test--on 'darwin (ian/platform-screenshot-command))))
  (should (equal "scrot -s %s" (platform-test--on 'linux (ian/platform-screenshot-command))))
  (should (equal "scrot -s %s" (platform-test--on 'wsl (ian/platform-screenshot-command))))
  (should-not (platform-test--on 'windows (ian/platform-screenshot-command))))

(provide 'platform-test)
;;; platform-test.el ends here
