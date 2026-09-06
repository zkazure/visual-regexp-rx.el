;;; rx-query-replace.el --- Interactive query-replace with rx regexps -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Kazure Zheng <kazurezheng@gmail.com>
;; Keywords: matching, lisp, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; Interactive query-replace where the regexp is written as an `rx'
;; form with live preview, instead of copying a regexp from
;; `re-builder' to `query-replace-regexp'.
;;
;;   M-x rx-query-replace  ; current buffer becomes the target
;;   ;; edit the rx form in *RE-Builder*, e.g. '(seq "foo" (group (+ digit)))
;;   C-c C-c               ; read replacement, then query-replace
;;   C-c C-k               ; quit
;;
;; This is a thin layer on top of `re-builder': it uses the RE
;; Builder's built-in `rx' syntax for editing, its live overlay
;; updates for the preview, and `perform-replace' for the
;; replacement.  Because it builds on `re-builder' internals
;; (`reb-re-syntax', `reb-update-regexp', `reb-auto-update', ...),
;; it requires Emacs 28.1 or later.

;;; Code:

(require 're-builder)
(require 'rx)

(defvar rx-query-replace-replacement-history nil
  "History of replacement strings for `rx-query-replace'.")

(defvar rx-query-replace--prev-syntax nil
  "Value of `reb-re-syntax' before entering `rx-query-replace'.")

(defun rx-query-replace--ensure-default ()
  "Replace the initial invalid `'()' with a valid empty rx form.
The RE Builder starts its buffer with `'()', which is not a valid
rx form.  Replace it with `'(seq)', a valid form matching the
empty string, and put point after the opening paren."
  (when (string= (buffer-string) "'()")
    (erase-buffer)
    (insert "'(seq)")
    (goto-char (+ 2 (point-min)))))

(defun rx-query-replace--perform ()
  "Run query-replace for the rx form in the RE Builder buffer.
The current buffer must be the RE Builder buffer.  Signals an
error if the rx form is invalid or compiles to an empty regexp."
  (reb-update-regexp)
  (let* ((target reb-target-buffer)
         (from (buffer-local-value 'reb-regexp target))
         (to (read-string "Replace with: " nil
                          'rx-query-replace-replacement-history)))
    (when (string-empty-p from)
      (error "Empty regexp"))
    (reb-assert-buffer-in-window)
    (select-window reb-target-window)
    ;; The preview highlights all matches from `point-min', so start
    ;; replacing there too (an active region still limits the replace).
    (goto-char (point-min))
    (perform-replace from to t t nil)
    ;; Replacing edited the target buffer; refresh the highlights so
    ;; the preview matches reality, then return to the RE Builder.
    (with-current-buffer (get-buffer reb-buffer)
      (reb-auto-update nil nil nil t))
    (let ((win (get-buffer-window (get-buffer reb-buffer))))
      (when (window-live-p win)
        (select-window win)))))

(defun rx-query-replace-submit ()
  "Query-replace using the rx form in the RE Builder buffer.
Reads a replacement string and runs `perform-replace' in the
target buffer, highlighting every match."
  (interactive)
  (condition-case err
      (rx-query-replace--perform)
    (error (message "Invalid rx: %s" (error-message-string err)))))

(defun rx-query-replace-quit ()
  "Quit `rx-query-replace'.
Restores the previous RE Builder syntax, deletes the overlays and
restores the window configuration."
  (interactive)
  (rx-query-replace-minor-mode -1)
  (when rx-query-replace--prev-syntax
    (setq reb-re-syntax rx-query-replace--prev-syntax
          rx-query-replace--prev-syntax nil))
  (reb-quit))

;;;###autoload
(defun rx-query-replace ()
  "Interactively construct an rx regexp and query-replace with it.
Makes the current buffer the \"target\" buffer and displays the
RE Builder buffer with `rx' syntax in another window.  As you edit
the rx form there, matches are highlighted in the target buffer.
Type \\[rx-query-replace-submit] to query-replace, or \
\\[rx-query-replace-quit] to quit."
  (interactive)
  (setq rx-query-replace--prev-syntax reb-re-syntax)
  (if (and (string= (buffer-name) reb-buffer)
           (reb-mode-buffer-p))
      ;; Already inside the RE Builder: keep its content, just make
      ;; sure the syntax is `rx' and (re)activate the minor mode.
      (progn
        (unless (eq reb-re-syntax 'rx)
          (reb-change-syntax 'rx))
        (rx-query-replace-minor-mode 1))
    (setq reb-re-syntax 'rx)
    (re-builder)
    (with-current-buffer (get-buffer reb-buffer)
      (rx-query-replace--ensure-default)
      (rx-query-replace-minor-mode 1))))

(defvar rx-query-replace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rx-query-replace-submit)
    (define-key map (kbd "C-c C-k") #'rx-query-replace-quit)
    ;; `reb-mode-map' binds `C-c C-c' to `reb-toggle-case'; our minor
    ;; mode overrides it, so offer the toggle on another key.
    (define-key map (kbd "C-c C-t") #'reb-toggle-case)
    map)
  "Keymap for `rx-query-replace-minor-mode'.")

(define-minor-mode rx-query-replace-minor-mode
  "Minor mode for `rx-query-replace', on top of the RE Builder.
Makes \\[rx-query-replace-submit] run a query-replace with the rx
form in the buffer, and \\[rx-query-replace-quit] quit."
  :lighter " rx-qr"
  :keymap rx-query-replace-mode-map)

(provide 'rx-query-replace)

;;; rx-query-replace.el ends here
