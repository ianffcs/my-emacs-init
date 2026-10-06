;;; clipboard-test.el --- Tests for the clipboard module -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads modules/core-os.el with `use-package' stubbed out and exercises the
;; clipboard adapter selection and routing.  No real clipboard tool is spawned.
;;
;; Run:
;;   emacs --batch -Q -l ert -l test/clipboard-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(test-helper-load-module 'core-os t)

(defmacro clipboard-test--env (wsl graphic env tools &rest body)
  "Run BODY with WSL, GRAPHIC, ENV (alist) and TOOLS (executables) stubbed."
  (declare (indent 4))
  `(let ((ian/wsl-p-cache ,wsl))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _f) ,graphic))
               ((symbol-function 'getenv)
                (lambda (v &rest _) (cdr (assoc v ,env))))
               ((symbol-function 'executable-find)
                (lambda (c &rest _) (and (member c ,tools) (concat "/bin/" c)))))
       ,@body)))

(ert-deftest clipboard-installed-as-interprogram-functions ()
  (should (eq interprogram-cut-function #'ian/clipboard-copy))
  (should (eq interprogram-paste-function #'ian/clipboard-paste)))

(ert-deftest clipboard-adapter-table ()
  (let ((wl '(("WAYLAND_DISPLAY" . "wayland-0")))
        (x '(("DISPLAY" . ":0")))
        (both '(("WAYLAND_DISPLAY" . "wayland-0") ("DISPLAY" . ":0")))
        (tools '("wl-copy" "xclip")))
    (should (eq 'wsl (clipboard-test--env t nil nil nil (ian/clipboard-adapter))))
    (should (eq 'wsl (clipboard-test--env t t both tools (ian/clipboard-adapter))))
    ;; The formerly broken case: GUI frame on Linux outside WSL.
    (should (eq 'gui (clipboard-test--env nil t both tools (ian/clipboard-adapter))))
    (should (eq 'gui (clipboard-test--env nil t nil nil (ian/clipboard-adapter))))
    (should (eq 'wayland (clipboard-test--env nil nil both tools (ian/clipboard-adapter))))
    (should (eq 'wayland (clipboard-test--env nil nil wl '("wl-copy") (ian/clipboard-adapter))))
    (should (eq 'x11 (clipboard-test--env nil nil x tools (ian/clipboard-adapter))))
    (should (eq 'x11 (clipboard-test--env nil nil both '("xclip") (ian/clipboard-adapter))))
    (should-not (clipboard-test--env nil nil wl '("xclip") (ian/clipboard-adapter)))
    (should-not (clipboard-test--env nil nil nil tools (ian/clipboard-adapter)))))

(ert-deftest clipboard-copy-gui-uses-gui-select-text ()
  (let (got)
    (cl-letf (((symbol-function 'gui-select-text) (lambda (s) (push s got)))
              ((symbol-function 'make-process) (lambda (&rest _) (error "spawned")))
              ((symbol-function 'start-process) (lambda (&rest _) (error "spawned"))))
      (clipboard-test--env nil t nil nil (ian/clipboard-copy "hi")))
    (should (equal got '("hi")))))

(ert-deftest clipboard-paste-gui-uses-gui-selection-value ()
  (cl-letf (((symbol-function 'gui-selection-value) (lambda () "from-gui")))
    (should (equal "from-gui" (clipboard-test--env nil t nil nil (ian/clipboard-paste))))))

(ert-deftest clipboard-copy-wsl-uses-clip-exe ()
  (let (started sent)
    (cl-letf (((symbol-function 'start-process)
               (lambda (_n _b prog &rest _) (setq started prog) 'proc))
              ((symbol-function 'process-send-string) (lambda (_p s) (setq sent s)))
              ((symbol-function 'process-send-eof) #'ignore)
              ((symbol-function 'gui-select-text) (lambda (&rest _) (error "wrong"))))
      (clipboard-test--env t t nil nil (ian/clipboard-copy "w")))
    (should (equal started "clip.exe"))
    (should (equal sent "w"))))

(ert-deftest clipboard-copy-process-adapters ()
  (dolist (case '((wayland (("WAYLAND_DISPLAY" . "w")) "wl-copy" ("wl-copy"))
                  (x11 (("DISPLAY" . ":0")) "xclip" ("xclip"))))
    (let (cmd sent (ian/clipboard-process nil))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args) (setq cmd (plist-get args :command)) 'proc))
                ((symbol-function 'process-live-p) (lambda (_) nil))
                ((symbol-function 'process-send-string) (lambda (_p s) (setq sent s)))
                ((symbol-function 'process-send-eof) #'ignore))
        (clipboard-test--env nil nil (nth 1 case) (nth 3 case)
          (ian/clipboard-copy "t")))
      (should (equal (car cmd) (nth 2 case)))
      (should (equal sent "t")))))

(ert-deftest clipboard-paste-process-adapter-runs-tool ()
  (let (prog)
    (cl-letf (((symbol-function 'call-process)
               (lambda (p &rest _) (setq prog p) (insert "pasted") 0)))
      (should (equal "pasted"
                     (clipboard-test--env nil nil '(("WAYLAND_DISPLAY" . "w")) '("wl-copy")
                       (ian/clipboard-paste)))))
    (should (equal prog "wl-paste"))))

(ert-deftest clipboard-nil-adapter-is-inert ()
  (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "spawned")))
            ((symbol-function 'call-process) (lambda (&rest _) (error "spawned")))
            ((symbol-function 'gui-select-text) (lambda (&rest _) (error "spawned"))))
    (clipboard-test--env nil nil nil nil
      (should-not (ian/clipboard-copy "x"))
      (should-not (ian/clipboard-paste)))))

(ert-deftest clipboard-yank-advice-copies-via-interface ()
  (let (got (kill-ring nil) (kill-ring-yank-pointer nil)
        interprogram-cut-function interprogram-paste-function)
    (cl-letf (((symbol-function 'ian/clipboard-copy) (lambda (s) (setq got s))))
      (kill-new "killed")
      (ian/yank-to-system-clipboard))
    (should (equal got "killed"))
    (should (advice-member-p #'ian/yank-to-system-clipboard 'yank))
    (should (advice-member-p #'ian/yank-to-system-clipboard 'yank-pop))))

(provide 'clipboard-test)
;;; clipboard-test.el ends here
