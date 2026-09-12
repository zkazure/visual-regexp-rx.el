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
(require 'rx)
(require 'visual-regexp)

;; Shared with visual-regexp-steroids; declared here so the
;; conditional definition below is still seen as special.
(defvar vr/engine)
;; Stage flag set by visual-regexp while the minibuffer is open.
(defvar vr--in-minibuffer)
;; Non-nil while the rx regexp minibuffer still holds the unedited
;; prefill; cleared by the first edit.
(defvar visual-regexp-rx--pristine nil)
;; Operator of the innermost rx form around point while completing.
;; It tells ambiguous names such as `whitespace' -- both a character
;; class and a syntax code -- which of the two is meant here.
(defvar visual-regexp-rx--completion-operator nil)

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

(defcustom visual-regexp-rx-completion t
  "Whether to offer completion while typing an rx form.
When non-nil, the regexp minibuffer in rx mode gets a
buffer-local entry in `completion-at-point-functions', so any
completion UI built on it (Corfu, Company with `company-capf',
...) pops up while you type, and `completion-at-point'
completes on demand.

The candidates are the built-in rx names plus the names defined
with `rx-define'.  They are filtered by their position in the
form, e.g. `(syntax ...)' only offers syntax codes."
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

;;; Completion at point

(defconst visual-regexp-rx--form-docs
  '(("seq" . "(seq RX...)\n\nMatch all of RX in order (also spelled `:').")
    ("sequence" . "(sequence RX...)\n\nMatch all of RX in order (alias of `seq').")
    (":" . "(: RX...)\n\nMatch all of RX in order (alias of `seq').")
    ("and" . "(and RX...)\n\nMatch all of RX simultaneously.")
    ("or" . "(or RX...)\n\nMatch any one of RX (also spelled `|').")
    ("|" . "(| RX...)\n\nMatch any one of RX (alias of `or').")
    ("any" . "(any SET...)\n\nMatch one character from the union of the SETs.")
    ("in" . "(in SET...)\n\nMatch one character from the SETs (alias of `any').")
    ("char" . "(char SET...)\n\nMatch one character from the SETs (alias of `any').")
    ("not-char" . "(not-char SET...)\n\nMatch one character that is not in the SETs.")
    ("not" . "(not RX...)\n\nMatch any string that RX do not match.")
    ("intersection" . "(intersection SET...)\n\nMatch one character in all of the SETs.")
    ("repeat" . "(repeat N RX) / (repeat MIN MAX RX)\n\nMatch RX N times, or between MIN and MAX times.")
    ("=" . "(= N RX)\n\nMatch RX exactly N times.")
    (">=" . "(>= N RX)\n\nMatch RX at least N times.")
    ("**" . "(** N M RX)\n\nMatch RX between N and M times.")
    ("zero-or-more" . "(zero-or-more RX)\n\nMatch RX zero or more times (also spelled `0+' and `*').")
    ("0+" . "(0+ RX)\n\nMatch RX zero or more times (alias of `zero-or-more').")
    ("*" . "(* RX)\n\nMatch RX zero or more times (alias of `zero-or-more').")
    ("one-or-more" . "(one-or-more RX)\n\nMatch RX one or more times (also spelled `1+' and `+').")
    ("1+" . "(1+ RX)\n\nMatch RX one or more times (alias of `one-or-more').")
    ("+" . "(+ RX)\n\nMatch RX one or more times (alias of `one-or-more').")
    ("zero-or-one" . "(zero-or-one RX)\n\nMatch RX zero or one time (also spelled `opt', `optional' and `?').")
    ("opt" . "(opt RX)\n\nMatch RX zero or one time (alias of `zero-or-one').")
    ("optional" . "(optional RX)\n\nMatch RX zero or one time (alias of `zero-or-one').")
    ("?" . "(? RX)\n\nMatch RX zero or one time (alias of `zero-or-one').")
    ("*?" . "(*? RX)\n\nMatch RX zero or more times, non-greedily.")
    ("+?" . "(+? RX)\n\nMatch RX one or more times, non-greedily.")
    ("??" . "(?? RX)\n\nMatch RX zero or one time, non-greedily.")
    ("minimal-match" . "(minimal-match RX...)\n\nMatch RX with non-greedy repetition by default.")
    ("maximal-match" . "(maximal-match RX...)\n\nMatch RX with greedy repetition by default.")
    ("group" . "(group RX...)\n\nGroup RX as the next numbered submatch (also spelled `submatch').")
    ("submatch" . "(submatch RX...)\n\nGroup RX as the next numbered submatch (alias of `group').")
    ("group-n" . "(group-n N RX...)\n\nGroup RX as submatch number N (also spelled `submatch-n').")
    ("submatch-n" . "(submatch-n N RX...)\n\nGroup RX as submatch number N (alias of `group-n').")
    ("backref" . "(backref N)\n\nMatch the text that submatch N matched.")
    ("syntax" . "(syntax CODE...)\n\nMatch one character whose syntax class is one of CODE.")
    ("not-syntax" . "(not-syntax CODE...)\n\nMatch one character whose syntax class is none of CODE.")
    ("category" . "(category CAT...)\n\nMatch one character in one of the Unicode categories CAT.")
    ("literal" . "(literal STRING)\n\nMatch STRING literally, without rx interpretation.")
    ("eval" . "(eval EXPR)\n\nMatch the regexp that EXPR evaluates to; EXPR runs when it compiles.")
    ("regexp" . "(regexp STRING)\n\nMatch STRING as a plain Emacs regexp (also spelled `regex').")
    ("regex" . "(regex STRING)\n\nMatch STRING as a plain Emacs regexp (alias of `regexp')."))
  "Documentation shown for the built-in rx forms.")

(defun visual-regexp-rx--strings (symbols)
  "Return the names of SYMBOLS, sorted and without duplicates."
  (sort (delete-dups (mapcar #'symbol-name symbols)) #'string-lessp))

(defun visual-regexp-rx--context ()
  "Return the rx context at point as (HEAD-P . OPERATOR).
HEAD-P is non-nil when point is on the first element of the
innermost enclosing list, where the rx operator is expected.
OPERATOR is the symbol at the head of that list, or nil when
point is outside any list."
  (let ((start (nth 1 (syntax-ppss))))
    (if (null start)
        (cons t nil)
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (symbol-start (or (car bounds) (point)))
             (head-start (save-excursion
                           (goto-char (1+ start))
                           (skip-chars-forward " \t\n")
                           (point))))
        (cons (= symbol-start head-start)
              (save-excursion
                (goto-char head-start)
                (let ((head (bounds-of-thing-at-point 'symbol)))
                  (and head
                       (intern (buffer-substring-no-properties
                                (car head) (cdr head)))))))))))

(defun visual-regexp-rx--defined-names ()
  "Return the symbols that `rx-define' has defined."
  (let (names)
    (mapatoms (lambda (symbol)
                (when (get symbol 'rx-definition)
                  (push symbol names))))
    names))

(defun visual-regexp-rx--names (&optional context)
  "Return the rx names to offer as completion candidates at point.
CONTEXT defaults to `visual-regexp-rx--context'."
  (let* ((context (or context (visual-regexp-rx--context)))
         (operator (cdr context)))
    (visual-regexp-rx--strings
     (if (car context)
         (append rx--builtin-forms rx--builtin-symbols
                 (visual-regexp-rx--defined-names))
       (cond
        ((memq operator '(syntax not-syntax))
         (mapcar #'car rx--syntax-codes))
        ((eq operator 'category)
         (mapcar #'car rx--categories))
        ((memq operator (list 'any 'in 'char 'not-char))
         (mapcar #'car rx--char-classes))
        (t
         (append rx--builtin-forms rx--builtin-symbols
                 (visual-regexp-rx--defined-names))))))))

(defun visual-regexp-rx--label (name)
  "Return the kind label shown for the rx name NAME.
Names that are both a character class and a syntax code, such as
`whitespace', are labelled according to the context stored in
`visual-regexp-rx--completion-operator'."
  (let ((symbol (intern name)))
    (cond
     ((and (memq visual-regexp-rx--completion-operator '(syntax not-syntax))
           (assq symbol rx--syntax-codes))
      "syntax")
     ((assq symbol rx--char-classes) "class")
     ((assq symbol rx--syntax-codes) "syntax")
     ((assq symbol rx--categories) "category")
     ((memq symbol rx--builtin-forms) "form")
     ((get symbol 'rx-definition) "rx-define")
     (t "symbol"))))

(defun visual-regexp-rx--kind (name)
  "Return the `:company-kind' of the rx name NAME."
  (pcase (visual-regexp-rx--label name)
    ((or "form" "rx-define") 'function)
    ((or "syntax" "category") 'keyword)
    (_ 'constant)))

(defun visual-regexp-rx--annotate (name)
  "Return the annotation shown after the rx name NAME."
  (format " %s" (visual-regexp-rx--label (substring-no-properties name))))

(defun visual-regexp-rx--translation (symbol)
  "Return the regexp that the rx SYMBOL translates to."
  (mapconcat #'identity (car (rx--translate symbol)) ""))

(defun visual-regexp-rx--definition-string (symbol)
  "Return the `rx-define' form that defines SYMBOL, as a string."
  (let ((definition (get symbol 'rx-definition)))
    (if (cdr definition)
        (format "`%s' is defined with `rx-define'.\n\n(rx-define %s %S %S)"
                symbol symbol (car definition) (cadr definition))
      (format "`%s' is defined with `rx-define'.\n\n(rx-define %s %S)"
              symbol symbol (car definition)))))

(defun visual-regexp-rx--doc-string (name)
  "Return the documentation text for the rx name NAME."
  (let* ((name (substring-no-properties name))
         (symbol (intern name)))
    (pcase (visual-regexp-rx--label name)
      ("form"
       (or (cdr (assoc name visual-regexp-rx--form-docs))
           (format "`%s' is a built-in rx form." name)))
      ("class"
       (format "`%s' is a character class.\n\nMatches the regexp `%s'."
               name (visual-regexp-rx--translation symbol)))
      ("syntax"
       (format "`%s' is a syntax code.\n\nUse it as `(syntax %s)'."
               name name))
      ("category"
       (format "`%s' is a Unicode category.\n\nUse it as `(category %s)'."
               name name))
      ("rx-define"
       (visual-regexp-rx--definition-string symbol))
      (_
       (format "`%s' is an rx symbol.\n\nMatches the regexp `%s'."
               name (visual-regexp-rx--translation symbol))))))

(defun visual-regexp-rx--doc-buffer (name)
  "Return a buffer with the documentation of the rx name NAME."
  (let ((text (visual-regexp-rx--doc-string name)))
    (when text
      (with-current-buffer (get-buffer-create " *visual-regexp-rx-doc*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (goto-char (point-min))
        (current-buffer)))))

(defun visual-regexp-rx--name-capf ()
  "Complete the rx name at point."
  (let* ((context (visual-regexp-rx--context))
         (bounds (bounds-of-thing-at-point 'symbol)))
    (setq visual-regexp-rx--completion-operator (cdr context))
    (list (or (car bounds) (point))
          (or (cdr bounds) (point))
          (visual-regexp-rx--names context)
          :display-sort-function #'identity
          :annotation-function #'visual-regexp-rx--annotate
          :company-kind #'visual-regexp-rx--kind
          :company-doc-buffer #'visual-regexp-rx--doc-buffer)))

(defun visual-regexp-rx--capf ()
  "Complete the rx name at point.
Return a `completion-at-point-functions' entry, or nil when
completion is disabled or point is not in an rx context."
  (when (and visual-regexp-rx-completion
             (eq vr/engine 'rx)
             (eq vr--in-minibuffer 'vr--minibuffer-regexp))
    (visual-regexp-rx--name-capf)))

(defun visual-regexp-rx--setup-completion ()
  "Register or unregister rx completion for this minibuffer.
The entry is added buffer-locally to
`completion-at-point-functions'; that is what makes completion
UIs such as Corfu find it.  Only our own entry is removed, so
functions added by other packages are left alone."
  (if (and visual-regexp-rx-completion
           (eq vr/engine 'rx)
           (eq vr--in-minibuffer 'vr--minibuffer-regexp))
      (progn
        (unless (local-variable-p 'completion-at-point-functions)
          ;; Do not inherit the global value: it only holds
          ;; `tags-completion-at-point-function', which is useless in
          ;; this minibuffer.
          (set (make-local-variable 'completion-at-point-functions) nil))
        (setq completion-at-point-functions
              (cons #'visual-regexp-rx--capf
                    (remq #'visual-regexp-rx--capf
                          completion-at-point-functions))))
    (remove-hook 'completion-at-point-functions #'visual-regexp-rx--capf t)))

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
highlights nothing.

In rx mode the completion at point function is registered as
well; see `visual-regexp-rx-completion'."
  (visual-regexp-rx--setup-completion)
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
