;;; visual-regexp-rx.el --- Extends visual-regexp to support rx notation -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kazure Zheng <kazurezheng@gmail.com>

;; Author: Kazure Zheng <kazurezheng@gmail.com>
;; Keywords: matching, lisp, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (visual-regexp "1.1"))
;; URL: https://github.com/zkazure/visual-regexp-rx.el

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

;; Extends visual-regexp to support rx notation, the way
;; visual-regexp-steroids extends it to support PCRE (Python
;; regular expressions).  Select the rx engine once:

;;   (setq vr/engine 'rx)

;; and type an rx form in the minibuffer instead of a regexp
;; string when running visual-regexp's own commands:

;;   M-x vr/query-replace
;;   (seq "TODO" (+ blank) (group (+ nonl)))

;; The form is compiled to a regexp string with `rx-to-string' and
;; handed to visual-regexp, so the live preview, the query loop and
;; all minibuffer shortcuts are unchanged.  Set `vr/engine' back to
;; `emacs' for plain visual-regexp behavior.

;;; Code:

(require 'cl-lib)
(require 'visual-regexp)

;; Shared with visual-regexp-steroids; declared here so the
;; conditional definition below is still seen as special.
(defvar vr/engine)
;; Stage flag set by visual-regexp while the minibuffer is open.
(defvar vr--in-minibuffer)
;; Non-nil once the rx regexp minibuffer has been prefilled; the
;; suppression itself is gated by a content check on the input.
(defvar visual-regexp-rx--pristine nil)

;;; The rx engine (shares `vr/engine' with visual-regexp-steroids)

(if (boundp 'vr/engine)
    ;; visual-regexp-steroids already defines it: just offer rx as an
    ;; additional engine choice.
    (cl-pushnew 'rx (get 'vr/engine 'custom-type)
                :test #'equal)
  (defcustom vr/engine 'emacs
    "Regexp engine used by visual-regexp.
`emacs' uses plain Emacs regexps; `rx' reads the input as an rx
form and compiles it with `rx-to-string'.

Set this to `rx' in your init file to use rx notation with
visual-regexp's own commands, e.g. (setq vr/engine 'rx)."
    :type '(choice (const emacs) (const rx))
    :group 'visual-regexp))

;;; Compile rx input

(defconst visual-regexp-rx--unmatchable "\\`a\\`"
  "Regexp guaranteed not to match any string.")

(defun visual-regexp-rx--fill-empty (form)
  "Replace empty `()' placeholders in FORM with `(seq)'.
Nested empty lists become `(seq)' too, so `(seq \"TODO\" ())'
compiles instead of erroring out while the form is under
construction."
  (cond
   ((null form) '(seq))
   ((consp form) (mapcar #'visual-regexp-rx--fill-empty form))
   (t form)))

(defun visual-regexp-rx--get-regexp-string (orig &optional for-display)
  "Compile the input as an rx form when `vr/engine' is `rx'.
ORIG is the original `vr--get-regexp-string'.  FOR-DISPLAY, when
non-nil, means the string is only shown, not used, so the raw
input is kept.

The pristine prefill `(seq \"\")' compiles to a regexp that
matches the empty string at every buffer position; while
`visual-regexp-rx--pristine' is non-nil and the minibuffer still
holds that prefill unedited, return a never-matching regexp
instead of flooding the buffer with zero-width highlights."
  (let ((regexp (funcall orig for-display)))
    (if (and (not for-display) (eq vr/engine 'rx))
        (if (and visual-regexp-rx--pristine
                 (string= regexp "(seq \"\")"))
            visual-regexp-rx--unmatchable
          (condition-case err
              (rx-to-string (visual-regexp-rx--fill-empty (read regexp)))
            (invalid-regexp (signal (car err) (cdr err))) ; rethrow unchanged
            (error (signal 'invalid-regexp (list "Invalid rx form")))))
      regexp)))

(advice-add 'vr--get-regexp-string :around #'visual-regexp-rx--get-regexp-string)

;;; Prefill the rx form

(defun visual-regexp-rx--minibuffer-setup ()
  "Prefill `(seq \"\")' on the regexp minibuffer in rx mode.
Point is left between the quotes, ready for typing.  Until the
minibuffer is edited, the empty form compiles to a never-matching
regexp, so the first rendering highlights nothing."
  (if (and (eq vr/engine 'rx)
           (eq vr--in-minibuffer 'vr--minibuffer-regexp))
      (progn
        ;; Set before the insert: visual-regexp's own after-change
        ;; function already runs during the insert and renders the
        ;; first feedback with this flag.
        (setq visual-regexp-rx--pristine t)
        (insert "(seq \"\")")
        (goto-char (- (point-max) 2))) ; point between the quotes
    (setq visual-regexp-rx--pristine nil)))

(add-hook 'minibuffer-setup-hook #'visual-regexp-rx--minibuffer-setup)

(provide 'visual-regexp-rx)
;;; visual-regexp-rx.el ends here
