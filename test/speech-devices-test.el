;;; speech-devices-test.el --- Tests for ffmpeg device parsing -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(test-helper-load-module "core-os" t)
(test-helper-load-module "tool-speech" t)

(defconst speech-devices-test--sample
  "ffmpeg version 7.0 Copyright (c) 2000-2024 the FFmpeg developers
  built with Apple clang version 15.0.0
[AVFoundation indev @ 0x7f8a4c004a80] AVFoundation video devices:
[AVFoundation indev @ 0x7f8a4c004a80] [0] FaceTime HD Camera
[AVFoundation indev @ 0x7f8a4c004a80] [1] Capture screen 0
[AVFoundation indev @ 0x7f8a4c004a80] AVFoundation audio devices:
[AVFoundation indev @ 0x7f8a4c004a80] [0] MacBook Pro Microphone
[AVFoundation indev @ 0x7f8a4c004a80] [1] External USB Mic
dummy: Input/output error
"
  "Realistic ffmpeg avfoundation -list_devices output.")

(ert-deftest speech-devices-parse-sections ()
  (let ((r (ian/parse-ffmpeg-devices speech-devices-test--sample)))
    (should (equal (car r) '((0 . "FaceTime HD Camera") (1 . "Capture screen 0"))))
    (should (equal (cadr r) '((0 . "MacBook Pro Microphone") (1 . "External USB Mic"))))))

(ert-deftest speech-devices-parse-empty ()
  (should (equal (ian/parse-ffmpeg-devices "") '(nil nil)))
  (should (equal (ian/parse-ffmpeg-devices "ffmpeg: command not found") '(nil nil))))

(ert-deftest speech-devices-defcustom-defined-everywhere ()
  (should (boundp 'ian/default-audio-device))
  (should (fboundp 'ian/select-default-audio-device)))

(ert-deftest speech-devices-select-noop-off-darwin ()
  (let ((ian/platform-override 'linux)
        (called nil))
    (cl-letf (((symbol-function 'ian/get-ffmpeg-device)
               (lambda () (setq called t) nil)))
      (ian/select-default-audio-device "x"))
    (should-not called)))

(ert-deftest speech-devices-select-on-darwin ()
  (let ((ian/platform-override 'darwin)
        (ian/default-audio-device nil))
    (cl-letf (((symbol-function 'ian/get-ffmpeg-device)
               (lambda () (ian/parse-ffmpeg-devices speech-devices-test--sample))))
      (ian/select-default-audio-device "External USB Mic"))
    (should (= ian/default-audio-device 1))))

;;; speech-devices-test.el ends here
