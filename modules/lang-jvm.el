;;; lang-jvm.el --- JVM Languages (Java, Kotlin, Scala) -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for JVM languages: Java, Kotlin, Scala, Groovy.
;; Migrated from README.org literate config.

;;; Code:

;; ============================================================================
;; 1. JAVA
;; ============================================================================

(use-package java-mode
  :straight (:type built-in)
  :hook ((java-mode . subword-mode)
         (java-mode . (lambda ()
                        (setq-local c-basic-offset 4)
                        (setq-local tab-width 4)))))

(use-package java-ts-mode
  :straight (:type built-in)
  :defer t)

;; Eglot Java (Eclipse JDT LS)

(use-package java-ts-mode
  :straight (:type built-in)
  :mode "\\.java\\'"
  :hook ((java-ts-mode . subword-mode)
         (java-ts-mode . (lambda ()
                           (setq-local c-basic-offset 4)
                           (setq-local tab-width 4)))))

(use-package eglot-java
  :after eglot
  :hook ((java-mode . eglot-java-mode)
         (java-ts-mode . eglot-java-mode))
  :custom
  (eglot-java-eclipse-jdt "jdtls")
  :config
  (eglot-java-init))

;; Groovy (for Gradle)
(use-package groovy-mode
  :mode (("\\.groovy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)
         ("Jenkinsfile\\'" . groovy-mode)))

;; ============================================================================
;; 2. KOTLIN
;; ============================================================================

(use-package kotlin-mode
  :hook (kotlin-mode . subword-mode))

(use-package kotlin-ts-mode
  :hook ((kotlin-ts-mode . subword-mode))
  :mode "\\.kt\\'"
  :straight (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :defer t)

;; Flycheck for Kotlin
(use-package flycheck-kotlin
  :disabled t
  ;; NOTE: This config uses Eglot + Flymake for diagnostics. Enable Flycheck if you want this.
  :after (flycheck kotlin-mode)
  :hook (kotlin-mode . flycheck-kotlin-setup))

;; ============================================================================
;; 3. SCALA
;; ============================================================================

(use-package scala-mode
  :interpreter "scala"
  :hook (scala-mode . subword-mode))

(use-package scala-ts-mode
  :hook ((scala-ts-mode . subword-mode))
  :mode "\\.scala\\'"
  :straight (:type built-in)
  :defer t)

;; SBT mode
(use-package sbt-mode
  :after scala-mode
  :commands sbt-start sbt-command
  :bind (:map scala-mode-map
              ("C-c s c" . sbt-command)
              ("C-c s s" . sbt-start)
              ("C-c s r" . sbt-run)
              ("C-c s t" . sbt-test))
  :custom
  (sbt:program-options '("-Dsbt.supershell=false")))

;; ============================================================================
;; 4. GRADLE
;; ============================================================================

(use-package gradle-mode
  :hook ((java-mode . gradle-mode)
         (kotlin-mode . gradle-mode))
  :bind (:map gradle-mode-map
              ("C-c g b" . gradle-build)
              ("C-c g r" . gradle-run)
              ("C-c g t" . gradle-test)
              ("C-c g c" . gradle-clean)))

;; ============================================================================
;; 5. MAVEN
;; ============================================================================

(use-package maven-test-mode
  :after java-mode
  :hook (java-mode . maven-test-mode)
  :bind (:map maven-test-mode-map
              ("C-c m t" . maven-test-method)
              ("C-c m f" . maven-test-file)
              ("C-c m a" . maven-test-all)))

;; ============================================================================
;; 6. ANDROID
;; ============================================================================

(defun ian/android-project-p ()
  "Check if current project is an Android project."
  (and (projectile-project-p)
       (or (file-exists-p (expand-file-name "AndroidManifest.xml" (projectile-project-root)))
           (file-exists-p (expand-file-name "app/build.gradle" (projectile-project-root))))))

(defun ian/android-build ()
  "Build Android project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "./gradlew assembleDebug")))

(defun ian/android-install ()
  "Install Android app on connected device."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "./gradlew installDebug")))

(defun ian/android-run ()
  "Build and run Android app."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "./gradlew installDebug && adb shell am start -n $(./gradlew -q :app:printDebugApplicationId)/$(./gradlew -q :app:printDebugMainActivity)")))

;; ============================================================================
;; 7. LSP CONFIGURATION
;; ============================================================================

(with-eval-after-load 'eglot
  ;; Java (Eclipse JDT LS) - handled by eglot-java

  ;; Kotlin
  (add-to-list 'eglot-server-programs
               '((kotlin-mode kotlin-ts-mode)
                 . ("kotlin-language-server")))

  ;; Scala (Metals)
  (add-to-list 'eglot-server-programs
               '((scala-mode scala-ts-mode)
                 . ("metals"))))

;; ============================================================================
;; 8. APHELEIA FORMATTERS
;; ============================================================================

(with-eval-after-load 'apheleia
  ;; Java (google-java-format)
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "-"))
  (setf (alist-get 'java-mode apheleia-mode-alist) '(google-java-format))
  (setf (alist-get 'java-ts-mode apheleia-mode-alist) '(google-java-format))

  ;; Kotlin (ktlint)
  (setf (alist-get 'ktlint apheleia-formatters)
        '("ktlint" "--format" "--stdin"))
  (setf (alist-get 'kotlin-mode apheleia-mode-alist) '(ktlint))

  ;; Scala (scalafmt)
  (setf (alist-get 'scalafmt apheleia-formatters)
        '("scalafmt" "--stdin"))
  (setf (alist-get 'scala-mode apheleia-mode-alist) '(scalafmt)))

;; ============================================================================
;; 9. TRANSIENT MENU
;; ============================================================================

(with-eval-after-load 'transient
  (transient-define-prefix ian/jvm-menu ()
                           "JVM development commands"
                           ["Gradle"
                            ("g b" "Build" gradle-build)
                            ("g r" "Run" gradle-run)
                            ("g t" "Test" gradle-test)
                            ("g c" "Clean" gradle-clean)]
                           ["Maven"
                            ("m t" "Test method" maven-test-method)
                            ("m f" "Test file" maven-test-file)
                            ("m a" "Test all" maven-test-all)]
                           ["SBT (Scala)"
                            ("s s" "Start" sbt-start)
                            ("s c" "Command" sbt-command)
                            ("s r" "Run" sbt-run)
                            ("s t" "Test" sbt-test)]
                           ["Android"
                            ("a b" "Build" ian/android-build)
                            ("a i" "Install" ian/android-install)
                            ("a r" "Run" ian/android-run)])

  (with-eval-after-load 'java-mode
    (define-key java-mode-map (kbd "C-c C-m") #'ian/jvm-menu))
  (with-eval-after-load 'kotlin-mode
    (define-key kotlin-mode-map (kbd "C-c C-m") #'ian/jvm-menu))
  (with-eval-after-load 'scala-mode
    (define-key scala-mode-map (kbd "C-c C-m") #'ian/jvm-menu)))

(provide 'lang-jvm)
;;; lang-jvm.el ends here
