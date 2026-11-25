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
;; Completion position flag: t at the head of a list, `top' outside
;; any list, nil in an argument position.
(defvar visual-regexp-rx--completion-head-p nil)
;; Cache of target buffer words: (BUFFER TICK . WORDS).
(defvar visual-regexp-rx--word-cache nil)

;;; Edit-the-buffer state

(defvar visual-regexp-rx--edit-active nil
  "Non-nil while the rx form is being read from the editing buffer.")

(defvar visual-regexp-rx--editing-buffer nil
  "Buffer the rx form is currently edited in, or nil.")

(defvar visual-regexp-rx--edit-window nil
  "Window the editing buffer is displayed in, or nil.
It is remembered on its own because the buffer can be killed in the
middle of a session, and its window would then be left behind.")

(defvar visual-regexp-rx--edit-aborted nil
  "Non-nil once the user abandoned the editing buffer.")

(defvar visual-regexp-rx--edit-message nil
  "Last message visual-regexp produced while editing in a buffer.")

(defvar-local visual-regexp-rx--edit-history-index nil
  "Position in the input history while cycling it, or nil.")

(defvar-local visual-regexp-rx--edit-history-draft nil
  "Unsaved editing buffer contents captured before browsing history.")

;;; The rx engine (shares `vr/engine' with visual-regexp-steroids)

(defun visual-regexp-rx--register-engine ()
  "Add `rx' to `vr/engine' choices when `vr/engine' is already defined."
  (let ((type (get 'vr/engine 'custom-type)))
    (when (and (consp type)
               (eq (car type) 'choice)
               (not (member '(const rx) (cdr type))))
      (put 'vr/engine 'custom-type (append type (list '(const rx)))))))

(if (boundp 'vr/engine)
    ;; visual-regexp-steroids already defines it: just offer rx as an
    ;; additional engine choice.
    (visual-regexp-rx--register-engine)
  (defcustom vr/engine 'emacs
    "Regexp engine used by visual-regexp.
`emacs' uses plain Emacs regexps; `rx' reads the input as an rx
form and compiles it with `rx-to-string'.

Set this to `rx' in your init file to use rx notation with
visual-regexp's own commands, e.g. (setq vr/engine 'rx)."
    :type '(choice (const emacs) (const rx))
    :group 'visual-regexp))

(defun visual-regexp-rx--after-load (file)
  "Re-register the `rx' choice when `visual-regexp-steroids' loads from FILE."
  (when (string-match-p "visual-regexp-steroids" file)
    (visual-regexp-rx--register-engine)))

(add-hook 'after-load-functions #'visual-regexp-rx--after-load)

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
form, e.g. `(syntax ...)' only offers syntax codes.

Inside a string literal, words from the buffer being searched
are offered instead, so the text to match can be completed as in
`isearch'.  That re-scans the buffer on every completion
request, so set this option to nil if it is too slow on large
buffers."
  :type 'boolean
  :group 'visual-regexp)

(defcustom visual-regexp-rx-use-editing-buffer nil
  "Whether to edit the regexp in a dedicated buffer.
When non-nil, the regexp prompt of the rx engine is answered in
`visual-regexp-rx--edit-buffer-name' instead of the minibuffer: the
form can then span several lines and is indented as Lisp, with the
live preview of the target buffer still visible below it.  The
replacement prompt keeps using the minibuffer.

That buffer is put in `visual-regexp-rx-edit-buffer-mode' with
`visual-regexp-rx-edit-mode' enabled; see the latter for its keys.
`RET' inserts a newline there instead of finishing, so multi-line
forms are typed naturally.

Everything else -- the live preview, the query loop and the
replacement prompt -- is unchanged."
  :type 'boolean
  :group 'visual-regexp)

(defcustom visual-regexp-rx-edit-buffer-mode 'lisp-data-mode
  "Major mode of the buffer used to edit the rx form.
The default, `lisp-data-mode', is the mode for buffers holding
data written in Lisp syntax: it indents Lisp and matches
parentheses, without the code semantics of `emacs-lisp-mode'.  It
is available since Emacs 28.1.

Set it to `emacs-lisp-mode' to get Elisp font-locking and
completion in that buffer as well, or to `prog-mode' or
`fundamental-mode' to keep it minimal, at the cost of no Lisp
indentation."
  :type 'function
  :group 'visual-regexp)

(defcustom visual-regexp-rx-edit-buffer-height 0.35
  "Height of the window showing the rx editing buffer.
A fraction of the frame height, or an integer number of lines; it
is passed to `display-buffer' as the `window-height' entry."
  :type 'number
  :group 'visual-regexp)

(defcustom visual-regexp-rx-allow-eval nil
  "Whether to allow `(eval EXPR)' forms in rx input.
Because live preview compiles the input on every edit, an `eval'
subform would be evaluated repeatedly while you type.  Keep this
nil unless you intend to run Lisp expressions during preview."
  :type 'boolean
  :group 'visual-regexp)

(defcustom visual-regexp-rx-word-completion-max-buffer-size 500000
  "Maximum target buffer size in characters for word completion.
When the searched buffer is larger than this limit, words are not
scanned inside rx string literals, while rx keyword completion
remains active.  Set to nil to scan buffers of any size."
  :type '(choice (integer :tag "Max characters")
                 (const :tag "No limit" nil))
  :group 'visual-regexp)

;;; Compile rx input

(defconst visual-regexp-rx--unmatchable "\\`a\\`"
  "Regexp guaranteed not to match any string.")

(defun visual-regexp-rx--fill-empty-tail (tail)
  "Recursively replace empty `()' placeholders in list TAIL.
Preserves improper list endings such as `(any (?a . ?z))' and
terminating nil."
  (cond
   ((null tail) nil)
   ((consp tail)
    (cons (visual-regexp-rx--fill-empty (car tail))
          (visual-regexp-rx--fill-empty-tail (cdr tail))))
   (t tail)))

(defun visual-regexp-rx--fill-empty (form)
  "Replace empty `()' placeholders in FORM with `(seq)'.
Nested empty lists become `(seq)' too, so `(seq \"TODO\" ())'
compiles instead of erroring out while the form is under
construction.  Improper lists such as `(any (?a . ?z))' are
preserved."
  (cond
   ((null form) '(seq))
   ((consp form)
    (cons (visual-regexp-rx--fill-empty (car form))
          (visual-regexp-rx--fill-empty-tail (cdr form))))
   (t form)))

(defun visual-regexp-rx--contains-eval-p (form)
  "Return non-nil when rx FORM contains an `(eval ...)' subform."
  (and (consp form)
       (or (eq (car form) 'eval)
           (let ((cur form)
                 (found nil))
             (while (and (consp cur) (not found))
               (when (visual-regexp-rx--contains-eval-p (car cur))
                 (setq found t))
               (setq cur (cdr cur)))
             found))))

(defun visual-regexp-rx--read-form (string)
  "Read a single Lisp form from STRING.
Signals `invalid-regexp' if STRING cannot be read or contains
non-whitespace text after the first form."
  (condition-case err
      (let* ((res (read-from-string string))
             (form (car res))
             (pos (cdr res)))
        (condition-case nil
            (progn
              (ignore (read-from-string string pos))
              (signal 'invalid-regexp (list "Trailing text after rx form")))
          (end-of-file form)))
    (invalid-regexp (signal (car err) (cdr err)))
    (error (signal 'invalid-regexp (list (error-message-string err))))))

(defun visual-regexp-rx--compile (string)
  "Compile rx form STRING into an Emacs regexp string.
Signals `invalid-regexp' on read or compilation errors."
  (let ((form (visual-regexp-rx--fill-empty
               (visual-regexp-rx--read-form string))))
    (when (and (not visual-regexp-rx-allow-eval)
               (visual-regexp-rx--contains-eval-p form))
      (signal 'invalid-regexp
              (list "rx `eval' form disabled (see `visual-regexp-rx-allow-eval')")))
    (condition-case err
        (rx-to-string form)
      (invalid-regexp (signal (car err) (cdr err)))
      (error (signal 'invalid-regexp (list (error-message-string err)))))))

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
          (visual-regexp-rx--compile regexp))
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

(defun visual-regexp-rx--syntax-ppss ()
  "Return `syntax-ppss' at point, ignoring any minibuffer prompt."
  (if (minibufferp)
      (save-restriction
        (narrow-to-region (minibuffer-prompt-end) (point-max))
        (syntax-ppss))
    (syntax-ppss)))

(defun visual-regexp-rx--context ()
  "Return the rx context at point as (HEAD-P . OPERATOR).
HEAD-P is t when point is on the first element of the innermost
enclosing list, `top' when outside any list, and nil in an
argument position.  OPERATOR is the symbol at the head of the
enclosing list, or nil outside any list."
  (save-restriction
    (when (minibufferp)
      (narrow-to-region (minibuffer-prompt-end) (point-max)))
    (let ((start (nth 1 (syntax-ppss))))
      (if (null start)
          (cons 'top nil)
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
                                  (car head) (cdr head))))))))))))

(defun visual-regexp-rx--defined-names (&optional kind)
  "Return the symbols that `rx-define' has defined.
When KIND is `form', return only parameterized definitions.
When KIND is `symbol', return only zero-argument definitions.
Otherwise return all defined names."
  (let (names)
    (mapatoms (lambda (symbol)
                (let ((def (get symbol 'rx-definition)))
                  (when (and def
                             (pcase kind
                               ('form (cdr def))
                               ('symbol (null (cdr def)))
                               (_ t)))
                    (push symbol names)))))
    names))

(defun visual-regexp-rx--names (&optional context)
  "Return the rx names to offer as completion candidates at point.
CONTEXT defaults to `visual-regexp-rx--context'."
  (let* ((context (or context (visual-regexp-rx--context)))
         (head-p (car context))
         (operator (cdr context)))
    (visual-regexp-rx--strings
     (cond
      ((eq head-p t)
       (append rx--builtin-forms
               (visual-regexp-rx--defined-names 'form)))
      ((eq head-p 'top)
       (append rx--builtin-symbols
               (visual-regexp-rx--defined-names 'symbol)))
      ((memq operator '(syntax not-syntax))
       (mapcar #'car rx--syntax-codes))
      ((eq operator 'category)
       (mapcar #'car rx--categories))
      ((memq operator (list 'any 'in 'char 'not-char 'intersection))
       (mapcar #'car rx--char-classes))
      (t
       (append rx--builtin-symbols
               (visual-regexp-rx--defined-names 'symbol)))))))

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
     ((and (eq symbol 'any)
           (not (eq visual-regexp-rx--completion-head-p t)))
      "symbol")
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
  (rx-to-string symbol t))

(defun visual-regexp-rx--definition-string (symbol)
  "Return the `rx-define' form of SYMBOL, as a string."
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
    (setq visual-regexp-rx--completion-head-p (car context)
          visual-regexp-rx--completion-operator (cdr context))
    (list (or (car bounds) (point))
          (or (cdr bounds) (point))
          (visual-regexp-rx--names context)
          :display-sort-function #'identity
          :annotation-function #'visual-regexp-rx--annotate
          :company-kind #'visual-regexp-rx--kind
          :company-doc-buffer #'visual-regexp-rx--doc-buffer)))

(defconst visual-regexp-rx--word-limit 200
  "Maximum number of words offered from the searched buffer.")

(defun visual-regexp-rx--word-bounds ()
  "Return the bounds of the word around point inside a string.
Return nil when there is no word character at point."
  (let* ((ppss (visual-regexp-rx--syntax-ppss))
         (str-start (nth 8 ppss))
         (base-limit (if (minibufferp) (minibuffer-prompt-end) (point-min)))
         (limit (if str-start (max base-limit (1+ str-start)) base-limit))
         (table (if (buffer-live-p vr--target-buffer)
                    (with-current-buffer vr--target-buffer (syntax-table))
                  (syntax-table)))
         (regexp "\\sw\\|\\s_"))
    (with-syntax-table table
      (when (or (looking-at regexp)
                (and (> (point) limit)
                     (save-excursion (forward-char -1) (looking-at regexp))))
        (cons (save-excursion
                (while (and (> (point) limit)
                            (save-excursion (forward-char -1)
                                            (looking-at regexp)))
                  (forward-char -1))
                (point))
              (save-excursion
                (while (looking-at regexp)
                  (forward-char 1))
                (point)))))))

(defun visual-regexp-rx--collect-buffer-words (buffer)
  "Return all distinct words in BUFFER, using a per-tick cache."
  (when (buffer-live-p buffer)
    (let ((tick (buffer-chars-modified-tick buffer)))
      (if (and visual-regexp-rx--word-cache
               (eq (car visual-regexp-rx--word-cache) buffer)
               (eql (cadr visual-regexp-rx--word-cache) tick))
          (cddr visual-regexp-rx--word-cache)
        (let ((words
               (with-current-buffer buffer
                 (when (or (null visual-regexp-rx-word-completion-max-buffer-size)
                           (<= (buffer-size)
                               visual-regexp-rx-word-completion-max-buffer-size))
                   (let ((seen (make-hash-table :test #'equal))
                         (result nil))
                     (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char (point-min))
                         (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" nil t)
                           (let ((word (match-string-no-properties 0)))
                             (unless (gethash word seen)
                               (puthash word t seen)
                               (push word result))))))
                     (nreverse result))))))
          (setq visual-regexp-rx--word-cache (cons buffer (cons tick words)))
          words)))))

(defun visual-regexp-rx--buffer-words (prefix)
  "Return the words of the searched buffer that start with PREFIX.
The words come from `vr--target-buffer', the buffer visual-regexp
is operating on."
  (when (buffer-live-p vr--target-buffer)
    (let ((fold (buffer-local-value 'case-fold-search vr--target-buffer))
          (words (visual-regexp-rx--collect-buffer-words vr--target-buffer))
          (matches nil)
          (count 0))
      (while (and words (< count visual-regexp-rx--word-limit))
        (let ((word (car words)))
          (when (string-prefix-p prefix word fold)
            (push word matches)
            (setq count (1+ count))))
        (setq words (cdr words)))
      (nreverse matches))))

(defun visual-regexp-rx--word-annotate (name)
  "Return the annotation shown after the buffer word NAME."
  (ignore name)
  " buffer")

(defun visual-regexp-rx--word-kind (name)
  "Return the `:company-kind' of the buffer word NAME."
  (ignore name)
  'text)

(defun visual-regexp-rx--word-capf ()
  "Complete a word from the searched buffer inside a string.
Return nil when point is not on a word inside a string."
  (let ((bounds (visual-regexp-rx--word-bounds)))
    (when (and bounds (< (car bounds) (cdr bounds)))
      (let ((words (visual-regexp-rx--buffer-words
                    (buffer-substring-no-properties
                     (car bounds) (cdr bounds)))))
        (when words
          (list (car bounds) (cdr bounds) words
                :annotation-function #'visual-regexp-rx--word-annotate
                :company-kind #'visual-regexp-rx--word-kind))))))

(defun visual-regexp-rx--capf ()
  "Complete an rx name or a buffer word at point.
Return a `completion-at-point-functions' entry, or nil when
completion is disabled or point is not in an rx context."
  (when (and visual-regexp-rx-completion
             (eq vr/engine 'rx)
             (eq vr--in-minibuffer 'vr--minibuffer-regexp))
    (if (nth 3 (visual-regexp-rx--syntax-ppss))
        (visual-regexp-rx--word-capf)
      (visual-regexp-rx--name-capf))))

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

;;; Edit the rx form in a buffer

(defconst visual-regexp-rx--edit-buffer-name "*visual-regexp-rx-edit*"
  "Name of the buffer in which the rx form is edited.")

(defun visual-regexp-rx--editing-buffer-enabled-p ()
  "Return non-nil when the regexp should be read from the editing buffer.
That is the case when `visual-regexp-rx-use-editing-buffer' is
non-nil, the rx engine is selected, and visual-regexp is prompting
for the regexp rather than for the replacement."
  (and visual-regexp-rx-use-editing-buffer
       (eq vr/engine 'rx)
       (eq vr--in-minibuffer 'vr--minibuffer-regexp)))

(defun visual-regexp-rx--update-header ()
  "Show the prompt and the latest message in the editing buffer.
The prompt comes from `vr--set-minibuffer-prompt', which
visual-regexp otherwise displays in the minibuffer, and the
message is the last one `visual-regexp-rx--edit-message' was set
to."
  (when (buffer-live-p visual-regexp-rx--editing-buffer)
    (with-current-buffer visual-regexp-rx--editing-buffer
      (setq header-line-format
            (concat (propertize (vr--set-minibuffer-prompt)
                                'face 'minibuffer-prompt)
                    (when visual-regexp-rx--edit-message
                      (concat " [" visual-regexp-rx--edit-message "]")))))))

(defun visual-regexp-rx--edit-after-change (&rest _)
  "Update the live preview after the rx editing buffer changed.
This mirrors what visual-regexp does on `after-change-functions' of
its own minibuffer, which cannot be reused because it is limited
to minibuffers."
  (when (and visual-regexp-rx--edit-active
             (eq vr--in-minibuffer 'vr--minibuffer-regexp))
    (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
      (unless (string= vr--last-minibuffer-contents contents)
        (setq vr--last-minibuffer-contents contents
              visual-regexp-rx--edit-message nil)
        (vr--show-feedback)
        (visual-regexp-rx--update-header)))))

(defun visual-regexp-rx--editing-minibuffer-message (orig message &rest args)
  "Show MESSAGE in the editing buffer instead of the echo area.
ORIG is `vr--minibuffer-message' and ARGS are its arguments.  When
the editing buffer is not in use, or when another minibuffer is
open in the middle of a session, the message is displayed exactly
as visual-regexp displays it."
  (if (not (and visual-regexp-rx--edit-active
                ;; The editing buffer is never a minibuffer, so a
                ;; message raised from one belongs to that minibuffer.
                (not (minibufferp))))
      (apply orig message args)
    (setq visual-regexp-rx--edit-message
          (if args (apply #'format message args) message))
    (visual-regexp-rx--update-header)))

(advice-add 'vr--minibuffer-message :around
            #'visual-regexp-rx--editing-minibuffer-message)

;; The editing buffer stands in for visual-regexp's regexp prompt, so
;; `vr--in-minibuffer' still names the regexp stage and visual-regexp's
;; two session hooks stay installed while it is in use.  Any minibuffer
;; opened in the meantime -- by `execute-extended-command',
;; `eval-expression', a `completing-read' -- is therefore mistaken for
;; that prompt.  The two advices below keep it out of the session; they
;; are inert as soon as the editing buffer is torn down, which happens
;; before the replacement prompt is read.

(defun visual-regexp-rx--editing-skip-minibuffer-setup (orig)
  "Do nothing while another minibuffer is opened during a session.
ORIG is `vr--minibuffer-setup'.  Left alone, it would rewrite the
prompt of that minibuffer as visual-regexp's own and show
visual-regexp's help in it."
  (unless visual-regexp-rx--edit-active
    (funcall orig)))

(advice-add 'vr--minibuffer-setup :around
            #'visual-regexp-rx--editing-skip-minibuffer-setup)

(defun visual-regexp-rx--editing-skip-after-change (orig beg end len)
  "Do nothing when another minibuffer is modified during a session.
ORIG is `vr--after-change'; BEG, END and LEN are the arguments it
expects.  ORIG only acts in minibuffers, so left alone it would
re-render the preview -- from the editing buffer -- while the user
types into that other minibuffer."
  (unless visual-regexp-rx--edit-active
    (funcall orig beg end len)))

(advice-add 'vr--after-change :around
            #'visual-regexp-rx--editing-skip-after-change)

(defun visual-regexp-rx--editing-get-regexp-string-full (orig)
  "Return the rx form being edited, or the value of ORIG.
ORIG is `vr--get-regexp-string-full'.  While the editing buffer is
in use, visual-regexp is still in its regexp stage, so ORIG would
call `minibuffer-contents' outside of a minibuffer."
  (if (and visual-regexp-rx--edit-active
           (buffer-live-p visual-regexp-rx--editing-buffer))
      (with-current-buffer visual-regexp-rx--editing-buffer
        (buffer-string))
    (funcall orig)))

(advice-add 'vr--get-regexp-string-full :around
            #'visual-regexp-rx--editing-get-regexp-string-full)

(defun visual-regexp-rx-edit-finish ()
  "Finish editing the rx form and return it to visual-regexp."
  (interactive)
  (exit-recursive-edit))

(defun visual-regexp-rx-edit-abort ()
  "Abort the visual-regexp command whose regexp is being edited."
  (interactive)
  (setq visual-regexp-rx--edit-aborted t)
  (exit-recursive-edit))

(defun visual-regexp-rx--edit-toggle-preview ()
  "Toggle the replacement preview, as `C-c p' does in the minibuffer."
  (interactive)
  (vr--shortcut-toggle-preview)
  (visual-regexp-rx--update-header))

(defvar visual-regexp-rx-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'visual-regexp-rx-edit-finish)
    (define-key map (kbd "C-c C-k") #'visual-regexp-rx-edit-abort)
    (define-key map (kbd "C-c ?") #'vr--minibuffer-help)
    (define-key map (kbd "C-c C-a") #'vr--shortcut-toggle-limit)
    (define-key map (kbd "C-c C-p") #'visual-regexp-rx--edit-toggle-preview)
    (define-key map (kbd "M-n") #'visual-regexp-rx-edit-history-next)
    (define-key map (kbd "M-p") #'visual-regexp-rx-edit-history-prev)
    map)
  "Keymap of `visual-regexp-rx-edit-mode'.")

(define-minor-mode visual-regexp-rx-edit-mode
  "Minor mode of the buffer used to edit the rx form.
It provides the keys the minibuffer provides for visual-regexp,
except that `RET' is left alone so that it inserts a newline and
the form can span several lines.

\\<visual-regexp-rx-edit-mode-map>\\
\\[visual-regexp-rx-edit-finish] finishes the input.
\\[visual-regexp-rx-edit-abort] aborts the whole command.
\\[vr--minibuffer-help] shows help.
\\[vr--shortcut-toggle-limit] and
\\[visual-regexp-rx--edit-toggle-preview] are the minibuffer
shortcuts, and \\[visual-regexp-rx-edit-history-prev] and
\\[visual-regexp-rx-edit-history-next] cycle earlier inputs."
  :lighter " VR-rx"
  :keymap visual-regexp-rx-edit-mode-map
  :group 'visual-regexp)

(defun visual-regexp-rx--create-message-overlay ()
  "Give `vr--minibuffer-message-overlay' an overlay the cleanup can delete.
`vr--interactive-get-args' deletes that overlay when it finishes
without checking first that it is one, and visual-regexp only ever
creates it while a minibuffer is in use.  Create it here instead, in
the editing buffer: it is never displayed, and once the buffer dies
the leftover dead overlay makes the cleanup skip its unguarded
delete."
  (unless (overlayp vr--minibuffer-message-overlay)
    (setq vr--minibuffer-message-overlay
          (make-overlay (point-min) (point-min)))))

(defun visual-regexp-rx--edit-buffer-display (buffer)
  "Display BUFFER in a side window at the bottom of the frame.
A side window keeps the target buffer visible for the live
preview.  Return the window used, or nil when there is none."
  (let ((alist `((display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . ,visual-regexp-rx-edit-buffer-height)
                 (dedicated . t)))
        window)
    (setq window (display-buffer buffer alist))
    (setq visual-regexp-rx--edit-window window)
    (when (window-live-p window)
      (select-window window))
    window))

(defun visual-regexp-rx--edit-buffer-setup ()
  "Prepare and return the buffer used to edit the rx form.
The buffer is put in `visual-regexp-rx-edit-buffer-mode', prefilled
with `visual-regexp-rx-prefill-form', and wired to update the live
preview on every change and to offer completion.

The session state is set before the prefill is inserted, so that
inserting it renders the preview once, just as visual-regexp does
when it enters its minibuffer.  That first render is what
`visual-regexp-rx-suppress-empty-highlight' keeps from flooding the
target buffer."
  (let ((buffer (get-buffer-create visual-regexp-rx--edit-buffer-name)))
    (setq visual-regexp-rx--editing-buffer buffer
          visual-regexp-rx--edit-aborted nil
          visual-regexp-rx--edit-message nil
          visual-regexp-rx--edit-active t)
    (with-current-buffer buffer
      (erase-buffer)
      ;; `delay-mode-hooks' keeps the mode hooks of the user's config
      ;; out of this transient buffer, and its `make-local-variable'
      ;; call is also what keeps Emacs from warning about a let-bound
      ;; `delay-mode-hooks' (see `kill-all-local-variables').
      (delay-mode-hooks
        (funcall visual-regexp-rx-edit-buffer-mode))
      ;; Indent after RET even when the global mode is turned off.
      (electric-indent-local-mode 1)
      (visual-regexp-rx-edit-mode 1)
      (add-hook 'after-change-functions #'visual-regexp-rx--edit-after-change
                nil t)
      (visual-regexp-rx--setup-completion)
      (visual-regexp-rx--create-message-overlay)
      (visual-regexp-rx--prefill)
      ;; Show the prompt even when the prefill rendered nothing.
      (visual-regexp-rx--update-header))
    buffer))

(defun visual-regexp-rx--edit-buffer-teardown ()
  "Remove the editing buffer, its window and its hooks."
  (let ((buffer visual-regexp-rx--editing-buffer)
        (window visual-regexp-rx--edit-window))
    (setq visual-regexp-rx--editing-buffer nil
          visual-regexp-rx--edit-window nil
          visual-regexp-rx--pristine nil
          visual-regexp-rx--word-cache nil
          ;; The session ends with the buffer it was read from; keeping
          ;; the two in step is what lets `--read-input' tell a read
          ;; that is already being served from one that is not.
          visual-regexp-rx--edit-active nil)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'after-change-functions
                     #'visual-regexp-rx--edit-after-change t)
        (remove-hook 'before-change-functions
                     #'visual-regexp-rx--clear-pristine t))
      (kill-buffer buffer))
    ;; The buffer may have been killed during the session, which can
    ;; leave its window behind; it is ours, so remove it as well.
    (when (window-live-p window)
      (delete-window window))))

(defun visual-regexp-rx--read-in-buffer ()
  "Read the rx form in the editing buffer and return it.
Signal `quit' when the user aborted it with
`visual-regexp-rx-edit-abort', or when the editing buffer was killed
in the middle of the session."
  (let ((previous-buffer (current-buffer))
        (previous-window (selected-window))
        ;; The separator of a search/replace pair must not stick to the
        ;; text typed after it; visual-regexp binds this around its own
        ;; minibuffer read.
        (text-property-default-nonsticky
         (cons '(separator . t) text-property-default-nonsticky))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (visual-regexp-rx--edit-buffer-setup))
          (visual-regexp-rx--edit-buffer-display buffer)
          ;; The input must happen in the editing buffer even when
          ;; there was no window to show it in.
          (set-buffer buffer)
          (recursive-edit)
          (cond
           ;; The buffer can be killed during the session, since
           ;; `kill-buffer' offers it as the buffer to kill by default.
           ;; There is nothing left to read then, so treat it as the
           ;; abort it is.
           ((not (buffer-live-p buffer)) (signal 'quit nil))
           (visual-regexp-rx--edit-aborted (signal 'quit nil))
           (t (with-current-buffer buffer
                (buffer-string)))))
      (visual-regexp-rx--edit-buffer-teardown)
      (when (window-live-p previous-window)
        (select-window previous-window))
      (when (buffer-live-p previous-buffer)
        (set-buffer previous-buffer)))))

(defun visual-regexp-rx--read-input (real-read args)
  "Read one input for visual-regexp.
REAL-READ is the original `read-from-minibuffer' and ARGS are the
arguments it was called with.  Only the regexp prompt uses the
editing buffer; the replacement prompt keeps using the minibuffer.

`read-from-minibuffer' stays shadowed for as long as
`vr--interactive-get-args' runs, so a minibuffer read started from
inside the editing session -- by `execute-extended-command', by
`eval-expression', by a `completing-read' -- lands here as well.
Such a read must go to the minibuffer: taking it over would start a
second editing session, whose setup erases the form being edited."
  (if (and (visual-regexp-rx--editing-buffer-enabled-p)
           ;; `visual-regexp-rx--edit-active' is non-nil exactly while
           ;; `visual-regexp-rx--read-in-buffer' runs; the stage that
           ;; `--editing-buffer-enabled-p' looks at is still the
           ;; regexp one then, so it cannot rule this out on its own.
           (not visual-regexp-rx--edit-active))
      (visual-regexp-rx--read-in-buffer)
    (apply real-read args)))

(defun visual-regexp-rx--around-interactive-get-args (orig &rest args)
  "Read the rx form in a buffer when the editing buffer is enabled.
ORIG is `vr--interactive-get-args' and ARGS are its arguments;
the editing buffer is used when
`visual-regexp-rx-use-editing-buffer' is non-nil and the rx engine
is selected.  `read-from-minibuffer' is shadowed for the dynamic
extent of the call only, so no other caller of it is affected."
  (if (not (and visual-regexp-rx-use-editing-buffer
                (eq vr/engine 'rx)))
      (apply orig args)
    (let ((real-read (symbol-function 'read-from-minibuffer)))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest rargs)
                   (visual-regexp-rx--read-input real-read rargs))))
        (apply orig args)))))

(advice-add 'vr--interactive-get-args :around
            #'visual-regexp-rx--around-interactive-get-args)

;;; Input history

(defun visual-regexp-rx--edit-history-elements ()
  "Return the inputs the history commands cycle through.
This mirrors the history visual-regexp offers in its minibuffer: the
search/replace pairs of `vr/query-replace-defaults-variable' first,
combined with `vr/match-separator-string' so that visual-regexp can
split them again, and then the regexps of
`vr/query-replace-from-history-variable'."
  (let ((separator (when vr/match-separator-string
                     (propertize "\0"
                                 'display vr/match-separator-string
                                 'separator t))))
    (append
     (when separator
       (mapcar (lambda (from-to)
                 (concat (car from-to)
                         separator
                         (query-replace-descr (cdr from-to))))
               (symbol-value vr/query-replace-defaults-variable)))
     (symbol-value vr/query-replace-from-history-variable))))

(defun visual-regexp-rx--edit-history-insert (element)
  "Replace the contents of the editing buffer with ELEMENT.
A search/replace pair keeps the separator property of ELEMENT, so
that visual-regexp splits it into a search and a replacement again."
  ;; Replacing the whole text is one change as far as the preview is
  ;; concerned, so that cycling the history does not render the empty
  ;; intermediate state.
  (combine-after-change-calls
    (erase-buffer)
    (insert element))
  (goto-char (point-max)))

(defun visual-regexp-rx--edit-history-cycle (step)
  "Replace the edited form with an earlier or later input.
STEP is how many entries to move, positive for earlier ones.
Earlier means further back in the history the minibuffer would
offer, which visual-regexp builds for that prompt only."
  (when (buffer-live-p visual-regexp-rx--editing-buffer)
    (with-current-buffer visual-regexp-rx--editing-buffer
      (let ((elements (visual-regexp-rx--edit-history-elements))
            (position visual-regexp-rx--edit-history-index)
            (count (abs step)))
        (cond
         ((> step 0)
          (when elements
            (unless position
              (setq visual-regexp-rx--edit-history-draft (buffer-string)))
            (setq position (min (1- (length elements))
                                (+ (if position position -1) count)))
            (setq visual-regexp-rx--edit-history-index position)
            (visual-regexp-rx--edit-history-insert (nth position elements))))
         ((and (< step 0) position)
          (let ((next (- position count)))
            (if (< next 0)
                (let ((draft (or visual-regexp-rx--edit-history-draft "")))
                  (setq visual-regexp-rx--edit-history-index nil
                        visual-regexp-rx--edit-history-draft nil)
                  (visual-regexp-rx--edit-history-insert draft))
              (setq visual-regexp-rx--edit-history-index next)
              (visual-regexp-rx--edit-history-insert (nth next elements))))))))))

(defun visual-regexp-rx-edit-history-prev (&optional arg)
  "Replace the edited form with an earlier input.
ARG defaults to 1.  This is what `previous-history-element' does in
visual-regexp's minibuffer."
  (interactive "p")
  (visual-regexp-rx--edit-history-cycle (or arg 1)))

(defun visual-regexp-rx-edit-history-next (&optional arg)
  "Replace the edited form with a later input.
ARG defaults to 1.  This is what `next-history-element' does in
visual-regexp's minibuffer."
  (interactive "p")
  (visual-regexp-rx--edit-history-cycle (- (or arg 1))))

;;; Prefill the rx form

(defun visual-regexp-rx--clear-pristine (&rest _)
  "Clear `visual-regexp-rx--pristine' and drop its one-shot hook.
Runs on `before-change-functions' of the buffer the rx form is read
from, and is called directly when there is no prefill at all."
  (setq visual-regexp-rx--pristine nil)
  (remove-hook 'before-change-functions #'visual-regexp-rx--clear-pristine t))

(defun visual-regexp-rx--prefill ()
  "Prefill the current rx input buffer with `visual-regexp-rx-prefill-form'.
An empty value means no prefill.  Point is left inside the first
empty string literal of the form, ready for typing, and at the end
of the form when it has none.

Until the buffer is edited and while `visual-regexp-rx--pristine'
is non-nil, `visual-regexp-rx--get-regexp-string' knows the form is
still the untouched prefill; see
`visual-regexp-rx-suppress-empty-highlight'."
  (if (string= "" visual-regexp-rx-prefill-form)
      (visual-regexp-rx--clear-pristine)
    (let ((start (point)))
      ;; Drop a leftover one-shot hook from an aborted session before
      ;; the prefill insert, so it cannot clear the flag on the insert
      ;; itself.
      (visual-regexp-rx--clear-pristine)
      ;; Set before the insert: visual-regexp's own after-change
      ;; function already runs during the insert and renders the
      ;; first feedback with this flag.
      (setq visual-regexp-rx--pristine t)
      (insert visual-regexp-rx-prefill-form)
      ;; Point between the quotes of the first empty string literal;
      ;; the natural spot to start typing.
      (goto-char start)
      (if (search-forward "\"\"" nil t)
          (backward-char)
        (goto-char (point-max)))
      ;; Clear on the first edit, not on the prefill insert itself.
      (add-hook 'before-change-functions #'visual-regexp-rx--clear-pristine
                nil t))))

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
well; see `visual-regexp-rx-completion'.

Nothing is done while the editing buffer is in use: visual-regexp
is still in its regexp stage, but a minibuffer being set up then is
not its prompt."
  (unless visual-regexp-rx--edit-active
    (visual-regexp-rx--setup-completion)
    (if (and (eq vr/engine 'rx)
             (eq vr--in-minibuffer 'vr--minibuffer-regexp))
        (visual-regexp-rx--prefill)
      (visual-regexp-rx--clear-pristine))))

(add-hook 'minibuffer-setup-hook #'visual-regexp-rx--minibuffer-setup)

(provide 'visual-regexp-rx)
;;; visual-regexp-rx.el ends here
