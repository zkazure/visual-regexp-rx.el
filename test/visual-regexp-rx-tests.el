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

;; Declared special so that the tests can `let'-bind the variables that
;; visual-regexp looks up with `symbol-value'.
(defvar vrx-tests--from-history nil)
(defvar vrx-tests--defaults nil)

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

(ert-deftest vrx-tests-edit-advices-installed ()
  "The two around advices of the editing buffer are installed."
  (should (advice-member-p #'visual-regexp-rx--around-interactive-get-args
                           'vr--interactive-get-args))
  (should (advice-member-p #'visual-regexp-rx--editing-get-regexp-string-full
                           'vr--get-regexp-string-full))
  (should (advice-member-p #'visual-regexp-rx--editing-minibuffer-message
                           'vr--minibuffer-message)))

(ert-deftest vrx-tests-edit-customs-defaults ()
  "The editing buffer options exist with their documented defaults."
  (should-not visual-regexp-rx-use-editing-buffer)
  (should (eq visual-regexp-rx-edit-buffer-mode 'lisp-data-mode))
  (should (equal visual-regexp-rx-edit-buffer-height 0.35))
  (dolist (var '(visual-regexp-rx-use-editing-buffer
                 visual-regexp-rx-edit-buffer-mode
                 visual-regexp-rx-edit-buffer-height))
    (should (member (list var 'custom-variable)
                    (get 'visual-regexp 'custom-group)))))

(ert-deftest vrx-tests-edit-enabled-p ()
  "The editing buffer is used for the rx regexp prompt only."
  (let ((visual-regexp-rx-use-editing-buffer t)
        (vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (should (visual-regexp-rx--editing-buffer-enabled-p)))
  (let ((visual-regexp-rx-use-editing-buffer nil)
        (vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (should-not (visual-regexp-rx--editing-buffer-enabled-p)))
  (let ((visual-regexp-rx-use-editing-buffer t)
        (vr/engine 'emacs)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (should-not (visual-regexp-rx--editing-buffer-enabled-p)))
  (let ((visual-regexp-rx-use-editing-buffer t)
        (vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-replace))
    (should-not (visual-regexp-rx--editing-buffer-enabled-p))))

(ert-deftest vrx-tests-edit-read-input-dispatch ()
  "`visual-regexp-rx--read-input' picks the buffer or the minibuffer."
  (let ((visual-regexp-rx-use-editing-buffer t)
        (vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (fallback-called nil))
    (cl-letf (((symbol-function 'visual-regexp-rx--read-in-buffer)
               (lambda () "BUFFER")))
      (should (equal (visual-regexp-rx--read-input
                      (lambda (&rest _) (setq fallback-called t) "MINIBUFFER")
                      nil)
                     "BUFFER"))
      (should-not fallback-called))
    ;; The replacement prompt keeps using the minibuffer.
    (let ((vr--in-minibuffer 'vr--minibuffer-replace))
      (should (equal (visual-regexp-rx--read-input
                      (lambda (&rest _) "MINIBUFFER") nil)
                     "MINIBUFFER")))
    ;; And so does everything when the option is off.
    (let ((visual-regexp-rx-use-editing-buffer nil)
          (vr--in-minibuffer 'vr--minibuffer-regexp))
      (should (equal (visual-regexp-rx--read-input
                      (lambda (&rest _) "MINIBUFFER") nil)
                     "MINIBUFFER")))))

(ert-deftest vrx-tests-edit-interactive-get-args-hijacks-read ()
  "`vr--interactive-get-args' reads the regexp from the buffer.
The shadowing of `read-from-minibuffer' is what makes the editing
buffer work, so it is exercised through a stand-in for visual-regexp.
It must not leak to the replacement prompt or to other callers."
  (let ((visual-regexp-rx-use-editing-buffer t)
        (vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (cl-letf (((symbol-function 'visual-regexp-rx--read-in-buffer)
               (lambda () "BUFFER"))
              ;; Stands in for `real-read': the fallback path returns
              ;; "MINIBUFFER" instead of prompting.
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "MINIBUFFER")))
      (should (equal (visual-regexp-rx--around-interactive-get-args
                      (lambda (&rest _) (read-from-minibuffer "p: ")))
                     "BUFFER"))
      (let ((vr--in-minibuffer 'vr--minibuffer-replace))
        (should (equal (visual-regexp-rx--around-interactive-get-args
                        (lambda (&rest _) (read-from-minibuffer "p: ")))
                       "MINIBUFFER")))
      ;; Nor while the editing buffer is the one being read: a
      ;; minibuffer read started from inside it is the minibuffer's.
      (let ((visual-regexp-rx--edit-active t))
        (should (equal (visual-regexp-rx--around-interactive-get-args
                        (lambda (&rest _) (read-from-minibuffer "p: ")))
                       "MINIBUFFER")))))
  ;; With the option off, the real reader is left alone.
  (let ((visual-regexp-rx-use-editing-buffer nil)
        (vr/engine 'rx))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "MINIBUFFER")))
      (should (equal (visual-regexp-rx--around-interactive-get-args
                      (lambda (&rest _) (read-from-minibuffer "p: ")))
                     "MINIBUFFER")))))

(defmacro vrx-tests--with-editing-buffer (&rest body)
  "Run BODY against the rx editing buffer and tear it down.
The state visual-regexp would have prepared for its prompt is
faked: `vr--last-minibuffer-contents' starts out empty, the way
`vr--set-regexp-string' leaves it, and the live preview is stubbed
out, since these tests have no target buffer to render into."
  (declare (indent 0) (debug t))
  `(let ((vr--last-minibuffer-contents ""))
     (cl-letf (((symbol-function 'vr--show-feedback) (lambda (&rest _) nil)))
       (unwind-protect
           (progn ,@body)
         (visual-regexp-rx--edit-buffer-teardown)))))

(ert-deftest vrx-tests-edit-buffer-mode-and-prefill ()
  "The editing buffer is set up in the configured mode and prefilled."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx--pristine nil))
    (vrx-tests--with-editing-buffer
      (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
        (should (eq (buffer-local-value 'major-mode buffer) 'lisp-data-mode))
        (should (equal (with-current-buffer buffer (buffer-string)) "(seq \"\")"))
        (should (= (with-current-buffer buffer (point)) 7))
        (should (buffer-local-value 'electric-indent-mode buffer))
        (should (buffer-local-value 'visual-regexp-rx-edit-mode buffer))
        (should visual-regexp-rx--pristine)
        ;; Multi-line forms are indented like any other Lisp data.
        (with-current-buffer buffer
          (erase-buffer)
          (insert "(seq \"a\"\n(+ digit)\n)")
          (indent-region (point-min) (point-max))
          (should (equal (buffer-string) "(seq \"a\"\n     (+ digit)\n     )")))))))

(ert-deftest vrx-tests-edit-buffer-enables-indent ()
  "RET indents in the editing buffer even with the global mode off."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (global electric-indent-mode))
    (unwind-protect
        (progn
          (set-default 'electric-indent-mode nil)
          (vrx-tests--with-editing-buffer
            (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
              (should (buffer-local-value 'electric-indent-mode buffer)))))
      (set-default 'electric-indent-mode global))))

(ert-deftest vrx-tests-edit-buffer-empty-prefill ()
  "An empty prefill form leaves the editing buffer empty."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx-prefill-form "")
        (visual-regexp-rx--pristine t))
    (vrx-tests--with-editing-buffer
      (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
        (should (equal (with-current-buffer buffer (buffer-string)) ""))
        (should (= (with-current-buffer buffer (point)) (point-min)))
        (should-not visual-regexp-rx--pristine)))))

(ert-deftest vrx-tests-edit-mode-hook-runs ()
  "Setting up the editing buffer runs `visual-regexp-rx-edit-mode-hook'.
That hook is where a buffer-local completion UI -- Corfu, say -- gets
enabled for a buffer the user's global modes never reach: the buffer is
set up with `delay-mode-hooks', so its major mode's hooks are delayed and
then dropped with the buffer, and global minor modes riding on
`after-change-major-mode-hook' never see it either."
  ;; `let*' matters here: an init form is evaluated in the enclosing
  ;; environment, so with a plain `let' the hook would record into some
  ;; other binding of SEEN and the assertion would pass on nothing.
  (let* ((vr/engine 'rx)
         (vr--in-minibuffer 'vr--minibuffer-regexp)
         (visual-regexp-rx-use-editing-buffer t)
         (vr--last-minibuffer-contents "")
         (seen nil)
         (fn (lambda ()
               (setq seen (list (buffer-name) visual-regexp-rx-edit-mode)))))
    (cl-letf (((symbol-function 'vr--show-feedback) (lambda (&rest _) nil)))
      (add-hook 'visual-regexp-rx-edit-mode-hook fn)
      (unwind-protect
          (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
            (should (equal seen (list (buffer-name buffer) t))))
        (remove-hook 'visual-regexp-rx-edit-mode-hook fn)
        (visual-regexp-rx--edit-buffer-teardown)))))

(ert-deftest vrx-tests-edit-buffer-mode-custom ()
  "`visual-regexp-rx-edit-buffer-mode' selects the major mode."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx-edit-buffer-mode 'prog-mode))
    (vrx-tests--with-editing-buffer
      (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
        (should (eq (buffer-local-value 'major-mode buffer) 'prog-mode))))))

(ert-deftest vrx-tests-edit-get-regexp-string-full ()
  "The regexp is read from the editing buffer while it is in use."
  (let ((orig (lambda () "ORIG"))
        (vr--in-minibuffer 'vr--minibuffer-regexp))
    (let ((visual-regexp-rx--edit-active nil)
          (visual-regexp-rx--editing-buffer nil))
      (should (equal (visual-regexp-rx--editing-get-regexp-string-full orig)
                     "ORIG")))
    (let ((buffer (generate-new-buffer " *vrx-edit*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "(seq \"a\")"))
            (let ((visual-regexp-rx--edit-active t)
                  (visual-regexp-rx--editing-buffer buffer))
              (should (equal
                       (visual-regexp-rx--editing-get-regexp-string-full orig)
                       "(seq \"a\")"))
              ;; A dead buffer never wins over the original.
              (kill-buffer buffer)
              (should (equal
                       (visual-regexp-rx--editing-get-regexp-string-full orig)
                       "ORIG"))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest vrx-tests-edit-after-change-rerenders ()
  "Changes in the editing buffer re-render the preview exactly once."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (vr--last-minibuffer-contents "")
        (renders 0))
    (vrx-tests--with-editing-buffer
      (cl-letf (((symbol-function 'vr--show-feedback)
                 (lambda (&rest _) (setq renders (1+ renders))))
                ((symbol-function 'visual-regexp-rx--update-header)
                 (lambda () nil)))
        (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
          (setq visual-regexp-rx--edit-active t)
          ;; The prefill itself renders once, which is what suppresses
          ;; the empty-match flood of the pristine prefill.
          (should (= renders 1))
          (should (equal vr--last-minibuffer-contents "(seq \"\")"))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "TODO"))
          (should (= renders 2))
          (should (equal vr--last-minibuffer-contents "(seq \"\")TODO"))
          ;; A no-op change does not render again.
          (with-current-buffer buffer
            (visual-regexp-rx--edit-after-change))
          (should (= renders 2)))))))

(ert-deftest vrx-tests-edit-after-change-inactive ()
  "Nothing is rendered while the editing buffer is not in use."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (vr--last-minibuffer-contents "")
        (renders 0))
    (with-temp-buffer
      (insert "(seq \"a\")")
      (cl-letf (((symbol-function 'vr--show-feedback)
                 (lambda (&rest _) (setq renders (1+ renders)))))
        (visual-regexp-rx--edit-after-change)
        (should (= renders 0))
        (should (equal vr--last-minibuffer-contents ""))
        (let ((visual-regexp-rx--edit-active t))
          (visual-regexp-rx--edit-after-change)
          (should (= renders 1)))))))

(ert-deftest vrx-tests-edit-message-goes-to-header ()
  "Messages of the editing session land in the header line."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (vr--calling-func 'vr--calling-func-query-replace)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx--edit-message nil))
    (vrx-tests--with-editing-buffer
      (let* ((buffer (visual-regexp-rx--edit-buffer-setup))
             (orig (lambda (message &rest _) message)))
        (let ((visual-regexp-rx--edit-active t))
          (visual-regexp-rx--editing-minibuffer-message orig "3 matches")
          (should (equal visual-regexp-rx--edit-message "3 matches"))
          (let ((header (buffer-local-value 'header-line-format buffer)))
            (should (string-match-p "3 matches" header))
            (should (string-match-p "Query replace" header))))
        ;; Outside the editing session the original is called.
        (let ((visual-regexp-rx--edit-active nil))
          (should (equal (visual-regexp-rx--editing-minibuffer-message
                          orig "other" "arg")
                         "other")))))))

(ert-deftest vrx-tests-edit-read-in-buffer-returns-contents ()
  "The edited form is returned, and the buffer is cleaned up."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t))
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda () (erase-buffer) (insert "(seq \"a\" (+ digit))")))
              ((symbol-function 'visual-regexp-rx--edit-buffer-display)
               (lambda (_buffer) nil)))
      (let ((result (visual-regexp-rx--read-in-buffer)))
        (should (equal result "(seq \"a\" (+ digit))"))
        (should-not visual-regexp-rx--edit-active)
        (should-not visual-regexp-rx--editing-buffer)
        (should-not (get-buffer visual-regexp-rx--edit-buffer-name))))))

(ert-deftest vrx-tests-edit-read-in-buffer-abort ()
  "`C-c C-k' aborts the command with `quit'."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx--edit-aborted nil))
    (cl-letf (((symbol-function 'recursive-edit)
               (lambda () (setq visual-regexp-rx--edit-aborted t)))
              ((symbol-function 'visual-regexp-rx--edit-buffer-display)
               (lambda (_buffer) nil)))
      (let ((quit-signalled nil))
        (condition-case nil
            (visual-regexp-rx--read-in-buffer)
          (quit (setq quit-signalled t)))
        (should quit-signalled))
      (should-not visual-regexp-rx--edit-active)
      (should-not (get-buffer visual-regexp-rx--edit-buffer-name)))))

(ert-deftest vrx-tests-edit-read-in-buffer-killed-buffer-aborts ()
  "Killing the editing buffer during a session aborts it.
`kill-buffer' offers the editing buffer as the buffer to kill by
default, and once it is gone there is nothing left to read.  Its
window must not be left behind either."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (vr--last-minibuffer-contents "")
        window)
    (unwind-protect
        (cl-letf (((symbol-function 'vr--show-feedback)
                   (lambda (&rest _) nil))
                  ((symbol-function 'recursive-edit)
                   (lambda ()
                     (setq window visual-regexp-rx--edit-window)
                     (kill-buffer visual-regexp-rx--editing-buffer))))
          (let ((quit-signalled nil))
            (condition-case nil
                (visual-regexp-rx--read-in-buffer)
              (quit (setq quit-signalled t)))
            (should quit-signalled))
          (should-not visual-regexp-rx--edit-active)
          (should-not visual-regexp-rx--editing-buffer)
          (should-not (get-buffer visual-regexp-rx--edit-buffer-name))
          ;; A dead window is still a window, so this shows the window
          ;; was really there rather than missed by a lookup.
          (should (windowp window))
          (should-not (window-live-p window)))
      (when (window-live-p window) (delete-window window))
      (visual-regexp-rx--edit-buffer-teardown))))

(ert-deftest vrx-tests-edit-other-minibuffer-read-is-not-taken-over ()
  "A minibuffer read started inside the session is left to the minibuffer.
`M-x', `M-:' and every `completing-read' read through
`read-from-minibuffer', which is shadowed for as long as
`vr--interactive-get-args' runs.  Taking such a read over starts a
second editing session: its setup erases the form being edited, and
the minibuffer that was asked for never appears."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer nil)
        (vr--calling-func nil)
        (vr--last-minibuffer-contents "")
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx-completion nil)
        (target (generate-new-buffer "*vrx-other-minibuffer*"))
        (sessions 0)
        (minibuffer-read-result nil)
        (form-after-read nil))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "TODO x\n")
            (goto-char (point-min)))
          (cl-letf (((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _) "ignore"))
                    ((symbol-function 'vr--show-feedback)
                     (lambda (&rest _) nil))
                    ((symbol-function 'recursive-edit)
                     (lambda ()
                       (setq sessions (1+ sessions))
                       (when (> sessions 1)
                         (error "a second editing session was started"))
                       ;; The form has been typed already.
                       (erase-buffer)
                       (insert "(seq \"typed\")")
                       ;; ... and now the user presses M-x.
                       (setq minibuffer-read-result
                             (read-from-minibuffer "M-x "))
                       (setq form-after-read (buffer-string)))))
            (vr--interactive-get-args 'vr--mode-regexp-replace
                                      'vr--calling-func-query-replace))
          (should (= sessions 1))
          (should (equal minibuffer-read-result "ignore"))
          (should (equal form-after-read "(seq \"typed\")")))
      (when (buffer-live-p target)
        (with-current-buffer target (set-buffer-modified-p nil))
        (kill-buffer target))
      (visual-regexp-rx--edit-buffer-teardown))))

(ert-deftest vrx-tests-edit-buffer-uses-side-window ()
  "The editing buffer is shown in a bottom side window.
That is what keeps the target buffer, and with it the live preview,
visible while the form is edited."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        window)
    (unwind-protect
        (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
          (setq window (visual-regexp-rx--edit-buffer-display buffer))
          (should (window-live-p window))
          (should (eq (window-parameter window 'window-side) 'bottom))
          (should (eq (window-buffer window) buffer)))
      (visual-regexp-rx--edit-buffer-teardown))
    (should-not (window-live-p window))
    (should-not (get-buffer visual-regexp-rx--edit-buffer-name))
    ;; Tearing the buffer down ends the session, buffer and flag
    ;; together; `--read-input' relies on that when it decides whether
    ;; a minibuffer read is its own.
    (should-not visual-regexp-rx--edit-active)))

(ert-deftest vrx-tests-edit-keymap-bindings ()
  "The editing buffer offers the minibuffer keys, but not `RET'."
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c C-c"))
              #'visual-regexp-rx-edit-finish))
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c C-k"))
              #'visual-regexp-rx-edit-abort))
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c ?"))
              #'vr--minibuffer-help))
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c C-a"))
              #'vr--shortcut-toggle-limit))
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c C-p"))
              #'visual-regexp-rx--edit-toggle-preview))
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "M-n"))
              #'visual-regexp-rx-edit-history-next))
  (should (eq (lookup-key visual-regexp-rx-edit-mode-map (kbd "M-p"))
              #'visual-regexp-rx-edit-history-prev))
  ;; `C-c <letter>' is reserved for the user, so it must stay free.
  (should-not (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c a")))
  (should-not (lookup-key visual-regexp-rx-edit-mode-map (kbd "C-c p")))
  (should-not (lookup-key visual-regexp-rx-edit-mode-map (kbd "RET"))))

(ert-deftest vrx-tests-edit-completion-not-overridden ()
  "Completion is added to the editing buffer without dropping others."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx-completion t))
    (vrx-tests--with-editing-buffer
      ;; The default mode has no completion, so ours is the only one.
      (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
        (should (buffer-local-value 'completion-at-point-functions buffer))
        (should (memq #'visual-regexp-rx--capf
                      (buffer-local-value 'completion-at-point-functions buffer))))
      (visual-regexp-rx--edit-buffer-teardown)
      ;; A mode that brings its own completion keeps it.
      (let ((visual-regexp-rx-edit-buffer-mode 'emacs-lisp-mode))
        (let ((buffer (visual-regexp-rx--edit-buffer-setup))
              (capfs nil))
          (setq capfs (buffer-local-value 'completion-at-point-functions buffer))
          (should (memq #'visual-regexp-rx--capf capfs))
          (should (memq 'elisp-completion-at-point capfs)))))))

(ert-deftest vrx-tests-edit-integration-end-to-end ()
  "`vr--interactive-get-args' takes the regexp from the editing buffer.
This drives the real call path of visual-regexp: the advice shadows
its minibuffer reader, the form is edited in a side window, the live
preview runs for real, and the replacement prompt still goes
through the minibuffer.  Only `recursive-edit' is stubbed, since the
test cannot wait for keys."
  (let ((vr/engine 'rx)
        (visual-regexp-rx-use-editing-buffer t)
        (vr--in-minibuffer nil)
        (vr--calling-func nil)
        (target (generate-new-buffer "*vrx-e2e-target*")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "TODO fix the login bug\nTODO write the docs"))
          (switch-to-buffer target)
          (cl-letf (((symbol-function 'recursive-edit)
                     (lambda ()
                       (erase-buffer)
                       (insert "(seq \"TODO\" (+ blank) (group (+ nonl)))")))
                    ;; The replacement prompt is not under test; the
                    ;; advice must hand it back to this reader.
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _) "DONE \\1")))
            (let ((args (vr--interactive-get-args
                         'vr--mode-regexp-replace
                         'vr--calling-func-query-replace)))
              (should (equal (nth 0 args)
                             "(seq \"TODO\" (+ blank) (group (+ nonl)))"))
              (should (equal (nth 1 args) "DONE \\1"))))
          ;; The edited form went into the from history.
          (should (member "(seq \"TODO\" (+ blank) (group (+ nonl)))"
                          (symbol-value vr/query-replace-from-history-variable)))
          ;; And the editing buffer is gone again.
          (should-not (get-buffer visual-regexp-rx--edit-buffer-name)))
      (when (buffer-live-p target)
        (with-current-buffer target (set-buffer-modified-p nil))
        (kill-buffer target))
      (visual-regexp-rx--edit-buffer-teardown))))

(ert-deftest vrx-tests-edit-message-overlay-is-real ()
  "The overlay visual-regexp deletes without checking is never nil.
`vr--interactive-get-args' runs `(unless (overlayp OVERLAY)
(delete-overlay OVERLAY))' when it finishes, and `delete-overlay'
rejects nil, so an editing session must leave an overlay behind."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (vr--minibuffer-message-overlay nil))
    (let (overlay)
      (vrx-tests--with-editing-buffer
        (visual-regexp-rx--edit-buffer-setup)
        (setq overlay vr--minibuffer-message-overlay)
        (should (overlayp overlay))
        (should (overlay-start overlay)))
      ;; The buffer it lived in is gone by then, and a dead overlay is
      ;; still an overlay as far as visual-regexp's cleanup is
      ;; concerned.
      (should-not (overlay-start overlay)))))

(ert-deftest vrx-tests-edit-history-prev-and-next ()
  "The history commands cycle the inputs the minibuffer would offer."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (vr/query-replace-from-history-variable 'vrx-tests--from-history)
        (vr/query-replace-defaults-variable 'vrx-tests--defaults)
        (vrx-tests--from-history '("(seq \"a\")" "(seq \"b\")"))
        (vrx-tests--defaults nil)
        (renders 0))
    (vrx-tests--with-editing-buffer
      (cl-letf (((symbol-function 'vr--show-feedback)
                 (lambda (&rest _) (setq renders (1+ renders)))))
        (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
          (should (= renders 1))        ; the prefill render
          ;; The most recent input comes first.
          (visual-regexp-rx-edit-history-prev)
          (should (equal (with-current-buffer buffer (buffer-string))
                         "(seq \"a\")"))
          (should (= renders 2))        ; cycling updates the preview
          (visual-regexp-rx-edit-history-prev)
          (should (equal (with-current-buffer buffer (buffer-string))
                         "(seq \"b\")"))
          ;; Going forward returns to the more recent one and stops
          ;; there instead of running off the end of the ring.
          (visual-regexp-rx-edit-history-next)
          (should (equal (with-current-buffer buffer (buffer-string))
                         "(seq \"a\")"))
          (visual-regexp-rx-edit-history-next)
          (should (equal (with-current-buffer buffer (buffer-string))
                         "(seq \"a\")"))
          (should (= (buffer-local-value
                      'visual-regexp-rx--edit-history-index buffer)
                     0)))))))

(ert-deftest vrx-tests-edit-history-pair-keeps-separator ()
  "A search/replace pair survives the round trip through the buffer.
visual-regexp finds the separator by its text property, so the
property has to make it into the buffer and back out again."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (visual-regexp-rx-use-editing-buffer t)
        (vr/query-replace-from-history-variable 'vrx-tests--from-history)
        (vr/query-replace-defaults-variable 'vrx-tests--defaults)
        (vrx-tests--from-history nil)
        (vrx-tests--defaults '(("TODO" . "DONE"))))
    (vrx-tests--with-editing-buffer
      (let ((buffer (visual-regexp-rx--edit-buffer-setup)))
        (cl-letf (((symbol-function 'vr--show-feedback) (lambda (&rest _) nil)))
          (visual-regexp-rx-edit-history-prev))
        (with-current-buffer buffer
          (should (text-property-any (point-min) (point-max) 'separator t))
          (should (equal (vr--query-replace--split-string (buffer-string))
                         '("TODO" . "DONE"))))))))

(ert-deftest vrx-tests-edit-history-outside-session-is-inert ()
  "The history commands never touch a buffer that is not being edited."
  (let ((visual-regexp-rx--editing-buffer nil)
        (vr/query-replace-from-history-variable 'vrx-tests--from-history)
        (vrx-tests--from-history '("(seq \"a\")")))
    (with-temp-buffer
      (insert "precious")
      (visual-regexp-rx-edit-history-prev)
      (visual-regexp-rx-edit-history-next)
      (should (equal (buffer-string) "precious")))))

(ert-deftest vrx-tests-edit-query-replace-end-to-end ()
  "A whole `vr/query-replace' runs with its regexp from the buffer.
The form is typed over several lines, the way the editing buffer
invites, and the query loop is driven to completion, so this covers
the interactive command end to end: the advice on
`vr--interactive-get-args', the editing buffer, a multi-line form, the
replacement prompt in the minibuffer, and the real replacement."
  (let ((vr/engine 'rx)
        (visual-regexp-rx-use-editing-buffer t)
        (vr--in-minibuffer nil)
        (vr--calling-func nil)
        (target (generate-new-buffer "*vrx-qr-target*")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "TODO fix the login bug\nTODO write the docs\n")
            (goto-char (point-min)))
          ;; The query loop makes the first match visible, which needs
          ;; the target buffer to be displayed.
          (switch-to-buffer target)
          (cl-letf (((symbol-function 'recursive-edit)
                     (lambda ()
                       (erase-buffer)
                       (insert "(seq \"TODO\"\n")
                       (insert "     (+ blank)\n")
                       (insert "     (group (+ nonl)))")))
                    ;; The replacement prompt must still be the
                    ;; minibuffer's.
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _) "DONE: \\1"))
                    ;; `?!' answers "replace all remaining matches".
                    ((symbol-function 'read-event) (lambda () ?!)))
            (call-interactively #'vr/query-replace))
          (should (equal (with-current-buffer target (buffer-string))
                         "DONE: fix the login bug\nDONE: write the docs\n")))
      (when (buffer-live-p target)
        (with-current-buffer target (set-buffer-modified-p nil))
        (kill-buffer target))
      (visual-regexp-rx--edit-buffer-teardown))))

(ert-deftest vrx-tests-edit-foreign-minibuffer-ignored ()
  "Another minibuffer opened during a session is left alone.
`vr--in-minibuffer' stays at the regexp stage while the editing buffer
is in use, so without this visual-regexp's setup and change hooks
would take any minibuffer -- the one M-x opens, M-:, a
`completing-read' -- for visual-regexp's own prompt: they would
prefill it, register the rx completion in it and re-render the preview
while the user types there."
  (let ((vr/engine 'rx)
        (vr--in-minibuffer 'vr--minibuffer-regexp)
        (vr--calling-func 'vr--calling-func-query-replace)
        (visual-regexp-rx-use-editing-buffer t)
        (visual-regexp-rx-completion t)
        (vr--last-minibuffer-contents "")
        (visual-regexp-rx--editing-buffer nil)
        (minibuffer (window-buffer (minibuffer-window)))
        (feedback-calls 0)
        (prompt-updates 0)
        (visual-regexp-rx--edit-active t))
    ;; `vr--interactive-get-args' installs these two for the session,
    ;; which is what makes them a problem here.
    (add-hook 'minibuffer-setup-hook 'vr--minibuffer-setup)
    (add-hook 'after-change-functions 'vr--after-change)
    (unwind-protect
        (progn
          ;; Opening a minibuffer runs `minibuffer-setup-hook'.
          (cl-letf (((symbol-function 'vr--show-feedback)
                     (lambda (&rest _) (setq feedback-calls (1+ feedback-calls))))
                    ((symbol-function 'vr--update-minibuffer-prompt)
                     (lambda () (setq prompt-updates (1+ prompt-updates)))))
            (with-current-buffer minibuffer
              (erase-buffer)
              (run-hooks 'minibuffer-setup-hook))
            (should (= prompt-updates 0))
            (should (equal (with-current-buffer minibuffer (buffer-string)) ""))
            (should-not (memq 'visual-regexp-rx--capf
                              (buffer-local-value
                               'completion-at-point-functions minibuffer)))
            ;; Typing in that minibuffer must not render the preview;
            ;; `after-change-functions' fires by itself on the insert.
            (with-current-buffer minibuffer (insert "vr/"))
            (should (= feedback-calls 0))
            (should (= prompt-updates 0))))
      (remove-hook 'minibuffer-setup-hook 'vr--minibuffer-setup)
      (remove-hook 'after-change-functions 'vr--after-change)
      (when (buffer-live-p minibuffer)
        (with-current-buffer minibuffer (erase-buffer))))
    ;; Once the session is over both hooks are used again.
    (let ((visual-regexp-rx--edit-active nil))
      (should (eq (visual-regexp-rx--editing-skip-minibuffer-setup
                   (lambda () 'setup-ran))
                  'setup-ran))
      (should (eq (visual-regexp-rx--editing-skip-after-change
                   (lambda (&rest _) 'change-ran) 1 2 0)
                  'change-ran)))))

(provide 'visual-regexp-rx-tests)






;;; visual-regexp-rx-tests.el ends here
