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

(ert-deftest vrx-tests-customs-defaults ()
  "The customization options exist with their documented defaults."
  (should (equal visual-regexp-rx-prefill-form "(seq \"\")"))
  (should (eq visual-regexp-rx-suppress-empty-highlight t))
  (should (member '(visual-regexp-rx-prefill-form custom-variable)
                  (get 'visual-regexp 'custom-group)))
  (should (member '(visual-regexp-rx-suppress-empty-highlight custom-variable)
                  (get 'visual-regexp 'custom-group))))

(ert-deftest vrx-tests-empty-prefill-form-disables-prefill ()
  "An empty `visual-regexp-rx-prefill-form' means no prefill."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil)
        (visual-regexp-rx-prefill-form ""))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (buffer-string) ""))
      (should-not visual-regexp-rx--pristine)
      (should-not (memq #'visual-regexp-rx--clear-pristine
                        before-change-functions)))))

(ert-deftest vrx-tests-prefill-form-custom ()
  "A custom prefill form is inserted with point inside its quotes."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil)
        (visual-regexp-rx-prefill-form "(group \"\")"))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (buffer-string) "(group \"\")"))
      (should (= (point) 9))
      (should visual-regexp-rx--pristine))))

(ert-deftest vrx-tests-prefill-form-no-empty-string-point-at-end ()
  "Without an empty string literal, point stays at the end."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil)
        (visual-regexp-rx-prefill-form "(seq)"))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (buffer-string) "(seq)"))
      (should (= (point) (point-max))))))

(ert-deftest vrx-tests-suppress-disabled-compiles-natively ()
  "With suppression off, the pristine prefill compiles natively."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil)
        (visual-regexp-rx-suppress-empty-highlight nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (equal (visual-regexp-rx--get-regexp-string
                      (lambda (&optional _) (buffer-string)))
                     (rx-to-string '(seq "")))))))

(ert-deftest vrx-tests-pristine-check-compares-prefill-form ()
  "A pristine flag only suppresses the exact prefill form."
  (let ((vr/engine 'rx)
        (visual-regexp-rx--pristine t)
        (visual-regexp-rx-prefill-form "(seq \"a\")"))
    (should (equal (visual-regexp-rx--get-regexp-string
                    (lambda (&optional _) "(seq \"b\")"))
                   (rx-to-string '(seq "b"))))))

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

(ert-deftest vrx-tests-completion-custom-default ()
  "The completion option exists with its documented default."
  (should (eq visual-regexp-rx-completion t))
  (should (member '(visual-regexp-rx-completion custom-variable)
                  (get 'visual-regexp 'custom-group))))

(ert-deftest vrx-tests-completion-registered-in-regexp-minibuffer ()
  "The rx regexp minibuffer gets a buffer-local completion function."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should (local-variable-p 'completion-at-point-functions))
      (should (memq #'visual-regexp-rx--capf completion-at-point-functions)))))

(ert-deftest vrx-tests-completion-not-registered-other-stages ()
  "Completion is only registered in the rx regexp minibuffer."
  (dolist (case '((emacs . vr--minibuffer-regexp)
                  (rx . vr--minibuffer-replace)))
    (let ((vr/engine (car case))
          (vr--in-minibuffer (cdr case))
          (visual-regexp-rx--pristine nil))
      (with-temp-buffer
        (visual-regexp-rx--minibuffer-setup)
        (should-not (memq #'visual-regexp-rx--capf
                          completion-at-point-functions)))))
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-completion nil)
        (visual-regexp-rx--pristine nil))
    (with-temp-buffer
      (visual-regexp-rx--minibuffer-setup)
      (should-not (memq #'visual-regexp-rx--capf
                        completion-at-point-functions)))))

(ert-deftest vrx-tests-completion-head-position ()
  "At the head of a list, all rx names are offered."
  (with-temp-buffer
    (insert "(seq \"a\" (gr")
    (goto-char (point-max))
    (let ((candidates (nth 2 (visual-regexp-rx--name-capf))))
      (should (member "group" candidates))
      (should (member "group-n" candidates))
      (should (member "seq" candidates)))))

(ert-deftest vrx-tests-completion-position-filters ()
  "Argument position restricts the offered names."
  (with-temp-buffer
    (insert "(seq (syntax wh")
    (goto-char (point-max))
    (let ((candidates (nth 2 (visual-regexp-rx--name-capf))))
      (should (member "whitespace" candidates))
      (should-not (member "group" candidates))))
  (with-temp-buffer
    (insert "(seq (category ch")
    (goto-char (point-max))
    (should (member "chinese" (nth 2 (visual-regexp-rx--name-capf)))))
  (with-temp-buffer
    (insert "(seq (any bl")
    (goto-char (point-max))
    (let ((candidates (nth 2 (visual-regexp-rx--name-capf))))
      (should (member "blank" candidates))
      (should-not (member "group" candidates)))))

(ert-deftest vrx-tests-completion-defined-names ()
  "Names defined with `rx-define' are offered and labelled."
  (unwind-protect
      (progn
        (rx-define vrx-tests-thing (seq "x"))
        (with-temp-buffer
          (insert "(seq vrx-tests-th")
          (goto-char (point-max))
          (should (member "vrx-tests-thing"
                          (nth 2 (visual-regexp-rx--name-capf)))))
        (should (equal (visual-regexp-rx--label "vrx-tests-thing")
                       "rx-define"))
        (should (string-match-p "rx-define"
                                (visual-regexp-rx--doc-string
                                 "vrx-tests-thing"))))
    (put 'vrx-tests-thing 'rx-definition nil)))

(ert-deftest vrx-tests-completion-label-and-kind ()
  "Candidates carry their rx kind."
  (should (equal (visual-regexp-rx--label "blank") "class"))
  (should (equal (visual-regexp-rx--label "group") "form"))
  (should (equal (visual-regexp-rx--label "string-quote") "syntax"))
  (should (equal (visual-regexp-rx--label "chinese") "category"))
  (should (equal (visual-regexp-rx--label "nonl") "symbol"))
  (should (eq (visual-regexp-rx--kind "blank") 'constant))
  (should (eq (visual-regexp-rx--kind "group") 'function))
  (should (eq (visual-regexp-rx--kind "string-quote") 'keyword))
  (should (equal (visual-regexp-rx--annotate "blank") " class"))
  ;; `whitespace' is both a character class and a syntax code, so the
  ;; label depends on the operator around point.
  (should (equal (visual-regexp-rx--label "whitespace") "class"))
  (should (equal (let ((visual-regexp-rx--completion-operator 'syntax))
                   (visual-regexp-rx--label "whitespace"))
                 "syntax"))
  (should (equal (let ((visual-regexp-rx--completion-operator 'any))
                   (visual-regexp-rx--label "whitespace"))
                 "class")))

(ert-deftest vrx-tests-completion-documentation ()
  "Candidates have documentation text and a documentation buffer."
  (should (string-match-p "\\[\\[:blank:\\]\\]"
                          (visual-regexp-rx--doc-string "blank")))
  (should (string-match-p "(group" (visual-regexp-rx--doc-string "group")))
  (should (string-match-p "syntax"
                          (visual-regexp-rx--doc-string "string-quote")))
  (should (bufferp (visual-regexp-rx--doc-buffer "group")))
  (kill-buffer " *visual-regexp-rx-doc*"))

(ert-deftest vrx-tests-completion-dispatch ()
  "`visual-regexp-rx--capf' only completes in the rx minibuffer."
  (let ((vr/engine 'emacs)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-completion t))
    (with-temp-buffer
      (insert "(seq gr")
      (goto-char (point-max))
      (should-not (visual-regexp-rx--capf))))
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-completion t))
    (with-temp-buffer
      (insert "(seq gr")
      (goto-char (point-max))
      (should (member "group" (nth 2 (visual-regexp-rx--capf)))))))

(ert-deftest vrx-tests-completion-buffer-words ()
  "Inside a string, words from the searched buffer are offered."
  (let* ((target (generate-new-buffer " *vrx-target*"))
         (vr--target-buffer target))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "TODO fix the login bug"))
          (with-temp-buffer
            (insert "(seq \"TO")
            (goto-char (point-max))
            (let ((capf (visual-regexp-rx--word-capf)))
              (should capf)
              (should (equal (nth 0 capf) 7))
              (should (equal (nth 1 capf) 9))
              (should (member "TODO" (nth 2 capf)))))
          (with-temp-buffer
            (insert "(seq \"lo")
            (goto-char (point-max))
            (should (member "login"
                            (nth 2 (visual-regexp-rx--word-capf))))))
      (kill-buffer target))))

(ert-deftest vrx-tests-completion-word-capf-nil-cases ()
  "No words are offered without a prefix or without a target buffer."
  (let ((vr--target-buffer nil))
    (with-temp-buffer
      (insert "(seq \"")
      (goto-char (point-max))
      (should-not (visual-regexp-rx--word-capf)))
    (with-temp-buffer
      (insert "(seq \"TO")
      (goto-char (point-max))
      (should-not (visual-regexp-rx--word-capf)))))

(ert-deftest vrx-tests-capf-string-dispatch ()
  "`visual-regexp-rx--capf' completes buffer words inside strings."
  (let* ((target (generate-new-buffer " *vrx-target*"))
         (vr--target-buffer target)
         (vr/engine 'rx)
         (vr--in-minibuffer 'vr--minibuffer-regexp)
         (visual-regexp-rx-completion t))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "TODO fix the login bug"))
          (with-temp-buffer
            (insert "(seq \"TO")
            (goto-char (point-max))
            (let ((capf (visual-regexp-rx--capf)))
              (should capf)
              (should (member "TODO" (nth 2 capf)))))
          (with-temp-buffer
            (insert "(seq gr")
            (goto-char (point-max))
            (should (member "group" (nth 2 (visual-regexp-rx--capf))))))
      (kill-buffer target))))

(provide 'visual-regexp-rx-tests)
;;; visual-regexp-rx-tests.el ends here
