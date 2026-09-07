;;; visual-regexp-rx-tests.el --- ERT tests for visual-regexp-rx -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kazure Zheng <kazurezheng@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the conversion layer and wiring.  The minibuffer-driven
;; visual-regexp flow itself is exercised manually against
;; test/sample.txt.

;;; Code:

(require 'ert)
(require 'visual-regexp-rx)

(ert-deftest vrx-tests-engine-defined ()
  "`vr/engine' exists and offers the rx choice."
  (should (boundp 'vr/engine))
  (should (member '(const rx) (cdr (get 'vr/engine 'custom-type)))))

(ert-deftest vrx-tests-advice-installed ()
  "The around advice is installed on `vr--get-regexp-string'."
  (should (advice-member-p #'visual-regexp-rx--get-regexp-string 'vr--get-regexp-string)))

(ert-deftest vrx-tests-converts-rx-form ()
  "With the rx engine, an rx form compiles to its regexp string."
  (let ((vr/engine 'rx))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\" (+ digit))"))
                   (rx-to-string '(seq "a" (+ digit)))))))

(ert-deftest vrx-tests-passthrough-emacs-engine ()
  "With the emacs engine, input is left untouched."
  (let ((vr/engine 'emacs))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\" (+ digit))"))
                   "(seq \"a\" (+ digit))"))))

(ert-deftest vrx-tests-passthrough-for-display ()
  "Display strings keep the raw input even with the rx engine."
  (let ((vr/engine 'rx))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\")") t)
                   "(seq \"a\")"))))

(ert-deftest vrx-tests-unbalanced-input-signals ()
  "Unbalanced input signals `invalid-regexp'."
  (let ((vr/engine 'rx))
    (should-error (visual-regexp-rx--get-regexp-string
                   (lambda (&optional _) "(seq \"a\""))
                  :type 'invalid-regexp)))

(ert-deftest vrx-tests-unknown-keyword-signals ()
  "Unknown rx keywords signal `invalid-regexp'."
  (let ((vr/engine 'rx))
    (should-error (visual-regexp-rx--get-regexp-string
                   (lambda (&optional _) "(foo)"))
                  :type 'invalid-regexp)))

(ert-deftest vrx-tests-prefill-regexp-minibuffer ()
  "The regexp minibuffer is prefilled with (seq \"\") in rx mode."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (buffer-string) "(seq \"\")"))
      (should (= (point) 7))
      (should visual-regexp-rx--pristine))))

(ert-deftest vrx-tests-prefill-pristine-unmatchable ()
  "The unedited prefill compiles to a never-matching regexp."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (visual-regexp-rx--get-regexp-string
                      (lambda (&optional _) (buffer-string)))
                     visual-regexp-rx--unmatchable)))))

(ert-deftest vrx-tests-edited-prefill-native ()
  "Editing the prefill clears the flag and compiles natively."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (insert "TODO")
      (should-not visual-regexp-rx--pristine)
      (should (equal (visual-regexp-rx--get-regexp-string
                      (lambda (&optional _) (buffer-string)))
                     (rx-to-string '(seq "TODO")))))))

(ert-deftest vrx-tests-pristine-cleared-on-first-edit ()
  "Editing the minibuffer clears the pristine flag and its hook."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (insert "T")
      (should-not visual-regexp-rx--pristine)
      (should-not (memq #'visual-regexp-rx--clear-pristine
                        before-change-functions)))))

(ert-deftest vrx-tests-empty-form-second-time-native ()
  "An empty form typed after editing compiles natively again."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (insert "TODO")
      (erase-buffer)
      (insert "(seq \"\")")
      (should (equal (visual-regexp-rx--get-regexp-string
                      (lambda (&optional _) (buffer-string)))
                     (rx-to-string '(seq "")))))))

(ert-deftest vrx-tests-empty-form-after-edit-native ()
  "Without the pristine flag, an empty form compiles natively."
  (let ((vr/engine 'rx)
        (visual-regexp-rx--pristine nil))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"\")"))
                   (rx-to-string '(seq ""))))))

(ert-deftest vrx-tests-no-pristine-other-stages ()
  "Non-rx minibuffer setups clear a leftover pristine flag."
  (let ((vr/engine 'emacs)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine t))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should-not visual-regexp-rx--pristine)))
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-replace)
        (visual-regexp-rx--pristine t))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should-not visual-regexp-rx--pristine))))

(ert-deftest vrx-tests-unmatchable-never-matches ()
  "`visual-regexp-rx--unmatchable' matches no string."
  (should-not (string-match-p visual-regexp-rx--unmatchable ""))
  (should-not (string-match-p visual-regexp-rx--unmatchable "anything")))

(ert-deftest vrx-tests-no-prefill-emacs-engine ()
  "No prefill with the emacs engine."
  (let ((vr/engine 'emacs)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (buffer-string) "")))))

(ert-deftest vrx-tests-no-prefill-replace-stage ()
  "No prefill on the replacement minibuffer."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-replace))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (buffer-string) "")))))

(ert-deftest vrx-tests-fill-empty-toplevel ()
  "An empty placeholder compiles as (seq)."
  (let ((vr/engine 'rx))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "()"))
                   (rx-to-string '(seq))))))

(ert-deftest vrx-tests-fill-empty-nested ()
  "Nested empty placeholders compile while keeping the form."
  (let ((vr/engine 'rx))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"TODO\" ())"))
                   (rx-to-string '(seq "TODO"))))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\" (group ()))"))
                   (rx-to-string '(seq "a" (group (seq))))))))

(ert-deftest vrx-tests-fill-empty-preserves-valid ()
  "Valid forms without placeholders are unchanged."
  (should (equal (visual-regexp-rx--fill-empty '(seq "a" (+ digit)))
                 '(seq "a" (+ digit))))
  (should (equal (visual-regexp-rx--fill-empty nil) '(seq)))
  (should (equal (visual-regexp-rx--fill-empty "string") "string")))

(provide 'visual-regexp-rx-tests)
;;; visual-regexp-rx-tests.el ends here
