;;; project-root-test.el --- Tests for ian/project-root -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(test-helper-load-module "core-utils" t)

(defmacro project-root-test--in-dir (dir &rest body)
  "Run BODY with `default-directory' DIR."
  `(let ((default-directory (file-name-as-directory ,dir)))
     ,@body))

(ert-deftest project-root-test-inside-project ()
  (let* ((root (make-temp-file "ian-proj" t))
         (sub (expand-file-name "src/deep" root)))
    (unwind-protect
        (progn
          (make-directory sub t)
          (make-directory (expand-file-name ".git" root))
          (project-root-test--in-dir
           sub
           (should (file-equal-p (ian/project-root) root))
           (should (file-directory-p (ian/project-root)))
           (should (file-equal-p (ian/with-project-root default-directory) root))))
      (delete-directory root t))))

(ert-deftest project-root-test-outside-project ()
  (let ((dir (make-temp-file "ian-noproj" t)))
    (unwind-protect
        (project-root-test--in-dir
         dir
         (should-not (project-current nil))
         (should (file-equal-p (ian/project-root) dir))
         (should (file-equal-p (ian/with-project-root default-directory) dir)))
      (delete-directory dir t))))

(ert-deftest project-root-test-never-nil ()
  (dolist (dir (list temporary-file-directory user-emacs-directory "/"))
    (project-root-test--in-dir
     dir
     (let ((root (ian/project-root)))
       (should (stringp root))
       (should (file-directory-p root))))))

(provide 'project-root-test)
;;; project-root-test.el ends here
