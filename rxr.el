;;; rxr.el --- Query-replace with rx forms and live preview -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Kazure Zheng <kazurezheng@gmail.com>
;; Keywords: matching, lisp, tools
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (visual-regexp "1.1"))

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

;; Interactive replacement where the regexp is written as an `rx'
;; form with live preview, instead of copying a regexp from
;; `re-builder' to `query-replace-regexp'.
;;
;;   M-x rxr-query-replace  ; current buffer becomes the target
;;   ;; edit the rx form in *RE-Builder*, e.g. '(seq "foo" (group (+ digit)))
;;   C-c C-c               ; type a replacement: every match previews it live
;;   RET                   ; confirm: y/n on each match, then the window closes
;;
;;   M-x rxr-replace        ; same, but RET replaces all matches at once
;;
;; The replacement phase is driven by visual-regexp: while you type
;; the replacement string in the minibuffer, the would-be result is
;; shown as an overlay on every match, without modifying the buffer.
;; RET confirms, C-g aborts and keeps the RE Builder open, C-c p
;; toggles the preview style and C-c a the match limit.
;;
;; This is a thin layer on top of `re-builder': it uses the RE
;; Builder's built-in `rx' syntax for editing and its live overlay
;; updates for the match preview, and visual-regexp's replacement
;; feedback and query loop for the replacement itself.  Because it
;; builds on `re-builder' internals (`reb-re-syntax',
;; `reb-update-regexp', `reb-auto-update', ...), it requires Emacs
;; 28.1 or later.

;;; Code:

(require 're-builder)
(require 'rx)
(require 'visual-regexp)

(defconst rxr-max-paren-recovery 20
  "Maximum number of closing parens tried by rx form recovery.")

(defun rxr--recover-read (re)
  "Read RE as an rx form, closing unbalanced parentheses.
While an rx form is being typed it is often temporarily
unbalanced.  Try appending up to
`rxr-max-paren-recovery' closing parens and return
the first form that parses.  Signal the original error if all
attempts fail."
  (condition-case orig-err
      (car (read-from-string re))
    (error
     (or (cl-loop for n from 1 to rxr-max-paren-recovery
                  for form = (ignore-errors
                               (car (read-from-string
                                     (concat re (make-string n ?\))))))
                  when form return form)
         (signal (car orig-err) (cdr orig-err))))))

(defun rxr--fill-empty (form)
  "Replace empty `()' placeholders with `(seq)' in the rx form FORM.
`()' is not a valid rx form, but while typing it is a natural
placeholder for a form that has not been filled in yet.  Treating
it as `(seq)' (matching the empty string) keeps the preview alive
without changing the meaning of any valid form."
  (cond ((null form) '(seq))
        ((consp form) (cons (car form)
                            (mapcar #'rxr--fill-empty (cdr form))))
        (t form)))

(defun rxr--cook-regexp (orig re)
  "Around-advice for `reb-cook-regexp' making rx previews tolerant.
Compiles rx forms with unbalanced parentheses and empty `()'
placeholders instead of blanking the highlighting while the form
is under construction."
  (if (eq reb-re-syntax 'rx)
      (let* ((form (rxr--recover-read re))
             (obj (eval form)))
        (rx-to-string (rxr--fill-empty obj)))
    (funcall orig re)))

(advice-add 'reb-cook-regexp :around #'rxr--cook-regexp)

(defface rxr-replacement
  '((((class color) (background light))
     :background "palegreen")
    (((class color) (background dark))
     :background "#2e4a2e")
    (t
     :inverse-video t))
  "Face used for the replacement preview.
visual-regexp shows the match and its replacement preview in the
same face, so this distinct face makes the preview recognizable."
  :group 'matching)

(defvar rxr--vr-session nil
  "Non-nil while a replacement session drives visual-regexp.")

(defun rxr--replacement-feedback (orig replacement match-data i)
  "Around-advice for `vr--do-replace-feedback-match-callback'.
Give the replacement preview its own face, so the match (visual-
regexp faces) and the would-be replacement (the distinct
`rxr-replacement' face) can be told apart.  Only
affects `rxr-query-replace' sessions; plain visual-regexp is
unchanged."
  (funcall orig replacement match-data i)
  (when rxr--vr-session
    (let ((ov (vr--get-overlay i 0)))
      (when (overlayp ov)
        (dolist (prop '(after-string display))
          (let ((s (overlay-get ov prop)))
            (when (stringp s)
              (overlay-put ov prop
                           (propertize (substring-no-properties s)
                                       'face 'rxr-replacement)))))))))

(advice-add 'vr--do-replace-feedback-match-callback
            :around #'rxr--replacement-feedback)

(defun rxr--delete-vr-overlays ()
  "Delete every visual-regexp overlay from the target buffer.
`vr--delete-overlays' only clears `vr--visible-overlays', but the
replacement feedback overlays are only tracked in the
`vr--overlays' hash table, so sweep that too."
  (maphash (lambda (_ij ov)
             (when (overlay-buffer ov)
               (delete-overlay ov)))
           vr--overlays))

(defvar rxr--prev-syntax nil
  "Value of `reb-re-syntax' before entering `rxr-query-replace'.")

(defvar-local rxr--replace-all nil
  "Whether `rxr-submit' replaces all matches at once.
Set by the entry commands `rxr-query-replace' and `rxr-replace'.")

(defun rxr--vr-read-replacement (target from bounds query)
  "Read a replacement string with visual-regexp live feedback.
Sets up the visual-regexp internals like `vr--interactive-get-args'
would (minus reading the regexp), reads the replacement from the
minibuffer and returns it.  TARGET is the buffer to replace in,
FROM the compiled regexp, BOUNDS the region limits (or nil), and
QUERY non-nil makes the prompt say \"Query replace\".  A `quit'
signal means the input was aborted."
  ;; Bounds must be computed in the target buffer: this function is
  ;; called with the RE Builder buffer current, whose `point-max' is
  ;; the length of the rx source, not of the target.
  (with-current-buffer target
    (setq vr--target-buffer-start (or (car bounds) (point-min))
          vr--target-buffer-end (or (cdr bounds) (point-max))))
  (setq vr--target-buffer target
        vr--regexp-string from
        vr--last-minibuffer-contents ""
        vr--calling-func (if query 'vr--calling-func-query-replace
                           'vr--calling-func-replace)
        vr--feedback-limit vr/default-feedback-limit
        vr--replace-preview vr/default-replace-preview)
  ;; Deactivate the mark in the target so the feedback faces are not
  ;; obscured by the region face.
  (with-current-buffer target
    (deactivate-mark))
  (add-hook 'after-change-functions #'vr--after-change)
  (add-hook 'minibuffer-setup-hook #'vr--minibuffer-setup)
  (unwind-protect
      (vr--set-replace-string)
    (setq vr--in-minibuffer nil)
    (remove-hook 'after-change-functions #'vr--after-change)
    (remove-hook 'minibuffer-setup-hook #'vr--minibuffer-setup)
    (setq vr--calling-func nil)
    (vr--delete-overlay-displays)
    (vr--delete-overlays)
    (rxr--delete-vr-overlays))
  vr--replace-string)

(defun rxr--region-bounds (target)
  "Return (beg . end) if TARGET shows an active region, else nil."
  (with-current-buffer target
    (when (region-active-p)
      (cons (region-beginning) (region-end)))))

(defun rxr--ensure-default ()
  "Replace the initial invalid `'()' with a valid empty rx form.
The RE Builder starts its buffer with `'()', which is not a valid
rx form.  Replace it with `'(seq)', a valid form matching the
empty string, and put point after the opening paren."
  (when (string= (buffer-string) "'()")
    (erase-buffer)
    (insert "'(seq)")
    (goto-char (+ 2 (point-min)))))

(defun rxr--perform (query)
  "Run the replacement for the rx form in the RE Builder buffer.
If QUERY is non-nil, ask for confirmation on every match with
visual-regexp's query loop; otherwise replace all matches at
once.  Reads the replacement string with a live preview in the
target buffer and closes the RE Builder when the replacement is
done.  Signals an error if the rx form is invalid or compiles to
an empty regexp; returns nil if the replacement input was
aborted."
  (reb-update-regexp)
  (let* ((target reb-target-buffer)
         (from (buffer-local-value 'reb-regexp target))
         (bounds (rxr--region-bounds target)))
    (when (string-empty-p from)
      (error "Empty regexp"))
    (condition-case nil
        (let ((rxr--vr-session t))
          (rxr--vr-read-replacement target from bounds query)
          (reb-assert-buffer-in-window)
          (select-window reb-target-window)
          ;; `vr--get-replacement' emulates `perform-replace''s
          ;; upper-case heuristic (`search-upper-case'), but the match
          ;; preview does not.  Bind it to nil so the replacement
          ;; always follows the preview, i.e. the target buffer's
          ;; `case-fold-search' (toggle with `reb-toggle-case').
          (let ((search-upper-case nil))
            (if query
                (vr--perform-query-replace)
              (vr--do-replace)))
          (rxr--delete-vr-overlays)
          (rxr-quit))
      (quit nil))))

(defun rxr-submit ()
  "Replace using the rx form in the RE Builder buffer.
Reads a replacement string with a live preview of every match,
then runs the replacement in the target buffer.  In
`rxr-query-replace' sessions, every match is confirmed with
visual-regexp's query loop; in `rxr-replace' sessions, all matches
are replaced at once.  The RE Builder window is closed when the
replacement is done; C-g while entering the replacement aborts
and keeps the RE Builder open."
  (interactive)
  (condition-case err
      (rxr--perform (not rxr--replace-all))
    (error (message "Invalid rx: %s" (error-message-string err)))))

(defun rxr-submit-all ()
  "Replace all matches of the rx form at once.
Like `rxr-submit', but never asks for confirmation
on individual matches."
  (interactive)
  (condition-case err
      (rxr--perform nil)
    (error (message "Invalid rx: %s" (error-message-string err)))))

(defun rxr-quit ()
  "Quit `rxr-query-replace'.
Restores the previous RE Builder syntax, deletes the overlays,
buries the RE Builder and restores the window configuration."
  (interactive)
  (when (buffer-live-p (get-buffer reb-buffer))
    (with-current-buffer (get-buffer reb-buffer)
      (rxr-mode -1)
      (reb-quit)))
  (when rxr--prev-syntax
    (setq reb-re-syntax rxr--prev-syntax
          rxr--prev-syntax nil)))

(defun rxr--enter (replace-all)
  "Enter the rx editor for interactive replacement.
The current buffer becomes the replacement target.  If
REPLACE-ALL is non-nil, `rxr-submit' replaces all
matches at once instead of querying."
  (setq rxr--prev-syntax reb-re-syntax)
  (if (and (string= (buffer-name) reb-buffer)
           (reb-mode-buffer-p))
      ;; Already inside the RE Builder: keep its content, just make
      ;; sure the syntax is `rx' and (re)activate the minor mode.
      (progn
        (unless (eq reb-re-syntax 'rx)
          (reb-change-syntax 'rx))
        (setq-local rxr--replace-all replace-all)
        (rxr-mode 1))
    (setq reb-re-syntax 'rx)
    (re-builder)
    (with-current-buffer (get-buffer reb-buffer)
      (rxr--ensure-default)
      (setq-local rxr--replace-all replace-all)
      (rxr-mode 1))))

;;;###autoload
(defun rxr-query-replace ()
  "Interactively construct an rx regexp and query-replace with it.
Makes the current buffer the \"target\" buffer and displays the
RE Builder buffer with `rx' syntax in another window.  As you edit
the rx form there, matches are highlighted in the target buffer.
Type \\[rxr-submit] to enter a replacement with a
live preview and then confirm every match,
\\[rxr-submit-all] to replace all matches at once,
or \\[rxr-quit] to quit.  See also `rxr-replace'."
  (interactive)
  (rxr--enter nil))

;;;###autoload
(defun rxr-replace ()
  "Interactively construct an rx regexp and replace its matches.
Like `rxr-query-replace', but \\[rxr-submit] replaces
all matches at once, without confirming every match."
  (interactive)
  (rxr--enter t))

(defvar rxr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rxr-submit)
    (define-key map (kbd "C-c C-r") #'rxr-submit-all)
    (define-key map (kbd "C-c C-k") #'rxr-quit)
    ;; `reb-mode-map' binds `C-c C-c' to `reb-toggle-case'; our minor
    ;; mode overrides it, so offer the toggle on another key.
    (define-key map (kbd "C-c C-t") #'reb-toggle-case)
    map)
  "Keymap for `rxr-mode'.")

(define-minor-mode rxr-mode
  "Minor mode for `rxr-query-replace', on top of the RE Builder.
Makes \\[rxr-submit] run the replacement for the rx
form in the buffer, and \\[rxr-quit] quit."
  :lighter " rxr"
  :keymap rxr-mode-map)

(provide 'rxr)

;;; rxr.el ends here
