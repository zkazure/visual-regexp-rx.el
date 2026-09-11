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
;; Non-nil while the rx regexp minibuffer still holds the unedited
;; prefill; cleared by the first edit.
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

;;; User options

(defcustom visual-regexp-rx-prefill-form "(seq \"\")"
  "Rx form prefilled in the regexp minibuffer in rx mode.
The form is inserted verbatim, so keep it on one line.  An empty
string disables the prefill and leaves the minibuffer empty, as
in plain visual-regexp.

When the form contains an empty string literal (\"\"), point is
left between its quotes, ready for typing; otherwise point ends
up after the form.

While the prefill is left unedited and
`visual-regexp-rx-suppress-empty-highlight' is non-nil, the text
is not compiled at all, so an invalid form is only reported once
you edit it."
  :type 'string
  :group 'visual-regexp)

(defcustom visual-regexp-rx-suppress-empty-highlight t
  "Whether the unedited prefill is kept from highlighting anything.
With the default `visual-regexp-rx-prefill-form' the pristine
prefill compiles to a regexp matching the empty string at every
buffer position, which visual-regexp renders as an empty-match
marker at each one.  While this is non-nil, that first render
uses a never-matching regexp instead; set it to nil to get
visual-regexp's own behavior.

Only the untouched prefill is affected: the flag is cleared on
the first edit and the input then compiles normally."
  :type 'boolean
  :group 'visual-regexp)

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

When `visual-regexp-rx-prefill-form' is the default, the
pristine prefill compiles to a regexp that matches the empty
string at every buffer position.  While
`visual-regexp-rx--pristine' is non-nil, the minibuffer still
holds `visual-regexp-rx-prefill-form' unedited and
`visual-regexp-rx-suppress-empty-highlight' is non-nil, return a
never-matching regexp instead of flooding the buffer with
zero-width highlights."
  (let ((regexp (funcall orig for-display)))
    (if (and (not for-display) (eq vr/engine 'rx))
        (if (and visual-regexp-rx--pristine
                 visual-regexp-rx-suppress-empty-highlight
                 (string= regexp visual-regexp-rx-prefill-form))
            visual-regexp-rx--unmatchable
          (condition-case err
              (rx-to-string (visual-regexp-rx--fill-empty (read regexp)))
            (invalid-regexp (signal (car err) (cdr err))) ; rethrow unchanged
            (error (signal 'invalid-regexp (list "Invalid rx form")))))
      regexp)))

(advice-add 'vr--get-regexp-string :around #'visual-regexp-rx--get-regexp-string)

;;; Prefill the rx form

(defun visual-regexp-rx--clear-pristine (&rest _)
  "Clear `visual-regexp-rx--pristine' after the first edit.
Runs on `before-change-functions' of the regexp minibuffer and
removes itself afterwards."
  (setq visual-regexp-rx--pristine nil)
  (remove-hook 'before-change-functions #'visual-regexp-rx--clear-pristine t))

(defun visual-regexp-rx--minibuffer-setup ()
  "Prefill the regexp minibuffer in rx mode.
The text inserted is `visual-regexp-rx-prefill-form'; an empty
value means no prefill.  Point is left inside the first empty
string literal of the form, ready for typing, and at the end of
the form when it has none.

Until the minibuffer is edited and while
`visual-regexp-rx-suppress-empty-highlight' is non-nil, the form
compiles to a never-matching regexp, so the first rendering
highlights nothing."
  (if (and (eq vr/engine 'rx)
           (eq vr--in-minibuffer 'vr--minibuffer-regexp)
           (not (string= "" visual-regexp-rx-prefill-form)))
      (let ((start (point)))
        ;; Drop a leftover one-shot hook from an aborted session
        ;; before the prefill insert, so it cannot clear the flag on
        ;; the insert itself.
        (remove-hook 'before-change-functions #'visual-regexp-rx--clear-pristine t)
        ;; Set before the insert: visual-regexp's own after-change
        ;; function already runs during the insert and renders the
        ;; first feedback with this flag.
        (setq visual-regexp-rx--pristine t)
        (insert visual-regexp-rx-prefill-form)
        ;; Point between the quotes of the first empty string
        ;; literal; the natural spot to start typing.
        (goto-char start)
        (if (search-forward "\"\"" nil t)
            (backward-char)
          (goto-char (point-max)))
        ;; Clear on the first edit, not on the prefill insert itself.
        (add-hook 'before-change-functions #'visual-regexp-rx--clear-pristine
                  nil t))
    (progn
      (setq visual-regexp-rx--pristine nil)
      (remove-hook 'before-change-functions #'visual-regexp-rx--clear-pristine t))))

(add-hook 'minibuffer-setup-hook #'visual-regexp-rx--minibuffer-setup)

(provide 'visual-regexp-rx)
;;; visual-regexp-rx.el ends here
