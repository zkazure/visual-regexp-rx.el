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

(ert-deftest vrx-tests-entry-commands ()
  "The rx entry points are interactive commands."
  (should (commandp 'vr/rx-query-replace))
  (should (commandp 'vr/rx-replace)))

(ert-deftest vrx-tests-advice-installed ()
  "The around advice is installed on `vr--get-regexp-string'."
  (should (advice-member-p #'vr/rx--get-regexp-string 'vr--get-regexp-string)))

(ert-deftest vrx-tests-converts-rx-form ()
  "With the rx engine, an rx form compiles to its regexp string."
  (let ((vr/engine 'rx))
    (should (equal (vr/rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\" (+ digit))"))
                   (rx-to-string '(seq "a" (+ digit)))))))

(ert-deftest vrx-tests-passthrough-emacs-engine ()
  "With the emacs engine, input is left untouched."
  (let ((vr/engine 'emacs))
    (should (equal (vr/rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\" (+ digit))"))
                   "(seq \"a\" (+ digit))"))))

(ert-deftest vrx-tests-passthrough-for-display ()
  "Display strings keep the raw input even with the rx engine."
  (let ((vr/engine 'rx))
    (should (equal (vr/rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\")") t)
                   "(seq \"a\")"))))

(ert-deftest vrx-tests-unbalanced-input-signals ()
  "Unbalanced input signals `invalid-regexp'."
  (let ((vr/engine 'rx))
    (should-error (vr/rx--get-regexp-string
                   (lambda (&optional _) "(seq \"a\""))
                  :type 'invalid-regexp)))

(ert-deftest vrx-tests-unknown-keyword-signals ()
  "Unknown rx keywords signal `invalid-regexp'."
  (let ((vr/engine 'rx))
    (should-error (vr/rx--get-regexp-string
                   (lambda (&optional _) "(foo)"))
                  :type 'invalid-regexp)))

(ert-deftest vrx-tests-prefill-regexp-minibuffer ()
  "The regexp minibuffer is prefilled with (seq \"\") in rx mode."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (with-temp-buffer
      (vr/rx--minibuffer-setup)
      (should (equal (buffer-string) "(seq \"\")"))
      (should (= (point) 7)))))

(ert-deftest vrx-tests-no-prefill-emacs-engine ()
  "No prefill with the emacs engine."
  (let ((vr/engine 'emacs)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (with-temp-buffer
      (vr/rx--minibuffer-setup)
      (should (equal (buffer-string) "")))))

(ert-deftest vrx-tests-no-prefill-replace-stage ()
  "No prefill on the replacement minibuffer."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-replace))
    (with-temp-buffer
      (vr/rx--minibuffer-setup)
      (should (equal (buffer-string) "")))))

(ert-deftest vrx-tests-fill-empty-toplevel ()
  "An empty placeholder compiles as (seq)."
  (let ((vr/engine 'rx))
    (should (equal (vr/rx--get-regexp-string
                    (lambda (&optional _) "()"))
                   (rx-to-string '(seq))))))

(ert-deftest vrx-tests-fill-empty-nested ()
  "Nested empty placeholders compile while keeping the form."
  (let ((vr/engine 'rx))
    (should (equal (vr/rx--get-regexp-string
                    (lambda (&optional _) "(seq \"TODO\" ())"))
                   (rx-to-string '(seq "TODO"))))
    (should (equal (vr/rx--get-regexp-string
                    (lambda (&optional _) "(seq \"a\" (group ()))"))
                   (rx-to-string '(seq "a" (group (seq))))))))

(ert-deftest vrx-tests-fill-empty-preserves-valid ()
  "Valid forms without placeholders are unchanged."
  (should (equal (vr/rx--fill-empty '(seq "a" (+ digit)))
                 '(seq "a" (+ digit))))
  (should (equal (vr/rx--fill-empty nil) '(seq)))
  (should (equal (vr/rx--fill-empty "string") "string")))

(ert-deftest vrx-tests-engine-bound-during-args-read ()
  "The rx engine is bound while the interactive args are read.
Regression: the entry points must use let* so `vr/engine' is
already rx when `vr--interactive-get-args' runs the minibuffer
(Emacs `let' evaluates every init form before binding)."
  (let ((seen nil)
        (vr--minibuffer-message-overlay (make-overlay 1 1)))
    (advice-add 'vr--set-regexp-string :around
                (lambda (orig &rest _) (setq seen vr/engine) "")
                '((name . vrx-test-intercept)))
    (advice-add 'vr--set-replace-string :around
                (lambda (orig &rest _) "")
                '((name . vrx-test-intercept-replace)))
    (advice-add 'vr/replace :around
                (lambda (orig &rest _) nil)
                '((name . vrx-test-intercept-replace-exec)))
    (unwind-protect
        (progn
          (vr/rx-replace)
          (should (eq seen 'rx)))
      (advice-remove 'vr--set-regexp-string 'vrx-test-intercept)
      (advice-remove 'vr--set-replace-string 'vrx-test-intercept-replace)
      (advice-remove 'vr/replace 'vrx-test-intercept-replace-exec))))

(provide 'visual-regexp-rx-tests)
;;; visual-regexp-rx-tests.el ends here
