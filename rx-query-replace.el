;;; rx-query-replace.el --- Interactive replacement with rx regexps -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Kazure Zheng <kazurezheng@gmail.com>
;; Keywords: matching, lisp, tools
;; Version: 0.2.0
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

;; Interactive replacement where the regexp is written as an `rx'
;; form with live preview, instead of copying a regexp from
;; `re-builder' to `query-replace-regexp'.
;;
;;   M-x rx-query-replace  ; current buffer becomes the target
;;   ;; edit the rx form in *RE-Builder*, e.g. '(seq "foo" (group (+ digit)))
;;   C-c C-c               ; type a replacement: every match previews it live
;;   RET                   ; confirm: y/n on each match, then the window closes
;;
;;   M-x rx-replace        ; same, but RET replaces all matches at once
;;
;; While you type the replacement string in the minibuffer, the
;; would-be result is shown as an overlay on every match in the
;; target buffer, without modifying it (the same approach as
;; visual-regexp).  C-g aborts the input and keeps the RE Builder
;; open.
;;
;; This is a thin layer on top of `re-builder': it uses the RE
;; Builder's built-in `rx' syntax for editing, its live overlay
;; updates for the match preview, and `perform-replace' for the
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

(defvar-local rx-query-replace--replace-all nil
  "Whether `rx-query-replace-submit' replaces all matches at once.
Set by the entry commands `rx-query-replace' and `rx-replace'.")

(defvar rx-query-replace--preview-overlays nil
  "Overlays showing the live replacement preview in the target buffer.")

(defvar rx-query-replace--minibuffer-state nil
  "Plist with the context of the replacement minibuffer session.
Holds :target, :from and :bounds.")

(defvar rx-query-replace-minibuffer-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c C-c") #'exit-minibuffer)
    (define-key map (kbd "C-c C-k") #'keyboard-quit)
    map)
  "Keymap used while entering the replacement string.")

(defun rx-query-replace--delete-preview-overlays ()
  "Delete all replacement preview overlays."
  (dolist (ov rx-query-replace--preview-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq rx-query-replace--preview-overlays nil))

(defun rx-query-replace--update-preview (&optional replacement)
  "Show the replacement preview in the target buffer.
REPLACEMENT defaults to the minibuffer contents.  Creates an
overlay on every match, displaying the would-be replacement
without modifying the buffer."
  (let* ((state rx-query-replace--minibuffer-state)
         (replacement (or replacement (minibuffer-contents-no-properties)))
         (target (plist-get state :target))
         (from (plist-get state :from))
         (bounds (plist-get state :bounds))
         (limit (or reb-auto-match-limit most-positive-fixnum)))
    (rx-query-replace--delete-preview-overlays)
    (condition-case err
        (save-excursion
          (with-current-buffer target
            (goto-char (or (car bounds) (point-min)))
            (let ((case-fold-search case-fold-search)
                  (nocasify (not (and case-replace case-fold-search)))
                  (count 0))
              (while (and (not (eobp))
                          (< count limit)
                          (re-search-forward from (or (cdr bounds) (point-max)) t))
                ;; Don't get stuck on zero-width matches.
                (when (and (= (match-beginning 0) (match-end 0))
                           (not (eobp)))
                  (forward-char 1))
                (let* ((repl (match-substitute-replacement replacement nocasify nil))
                       (ov (make-overlay (match-beginning 0) (match-end 0) target)))
                  (overlay-put ov 'priority 1001)
                  (if (= (match-beginning 0) (match-end 0))
                      (overlay-put ov 'after-string (propertize repl 'face 'reb-match-0))
                    (overlay-put ov 'display (propertize repl 'face 'reb-match-0)))
                  (push ov rx-query-replace--preview-overlays))
                (setq count (1+ count))))))
      (error (minibuffer-message (format " %s" (error-message-string err)))))))

(defun rx-query-replace--after-change (&rest _)
  "Update the replacement preview when the minibuffer changes."
  (when (and rx-query-replace--minibuffer-state (minibufferp))
    ;; Browsing the history momentarily empties the minibuffer; skip
    ;; that flicker (same guard as visual-regexp).
    (unless (and (string= "" (minibuffer-contents-no-properties))
                 (eq last-command 'previous-history-element))
      (rx-query-replace--update-preview))))

(defun rx-query-replace--read-replacement (target from bounds)
  "Read a replacement string, previewing it live in TARGET.
FROM is the compiled regexp and BOUNDS the region limits, or nil
for the whole buffer.  While the user types, the would-be
replacement is shown as overlays on every match, without
modifying TARGET.  A normal return means the replacement was
confirmed; a `quit' signal means it was aborted."
  (setq rx-query-replace--minibuffer-state
        (list :target target :from from :bounds bounds))
  (unwind-protect
      (minibuffer-with-setup-hook
          (lambda ()
            (add-hook 'after-change-functions #'rx-query-replace--after-change nil t)
            (rx-query-replace--update-preview))
        (read-from-minibuffer "Replace with: " nil
                              rx-query-replace-minibuffer-keymap
                              nil 'rx-query-replace-replacement-history))
    (rx-query-replace--delete-preview-overlays)
    (setq rx-query-replace--minibuffer-state nil)))

(defun rx-query-replace--region-bounds (target)
  "Return (beg . end) if TARGET shows an active region, else nil."
  (with-current-buffer target
    (when (region-active-p)
      (cons (region-beginning) (region-end)))))

(defun rx-query-replace--ensure-default ()
  "Replace the initial invalid `'()' with a valid empty rx form.
The RE Builder starts its buffer with `'()', which is not a valid
rx form.  Replace it with `'(seq)', a valid form matching the
empty string, and put point after the opening paren."
  (when (string= (buffer-string) "'()")
    (erase-buffer)
    (insert "'(seq)")
    (goto-char (+ 2 (point-min)))))

(defun rx-query-replace--perform (query)
  "Run the replacement for the rx form in the RE Builder buffer.
If QUERY is non-nil, ask for confirmation on every match with
`perform-replace'; otherwise replace all matches at once.  Reads
the replacement string with a live preview in the target buffer
and closes the RE Builder when the replacement is done.  Signals
an error if the rx form is invalid or compiles to an empty
regexp; returns nil if the replacement input was aborted."
  (reb-update-regexp)
  (let* ((target reb-target-buffer)
         (from (buffer-local-value 'reb-regexp target))
         (bounds (rx-query-replace--region-bounds target)))
    (when (string-empty-p from)
      (error "Empty regexp"))
    (condition-case nil
        (let ((to (rx-query-replace--read-replacement target from bounds)))
          (reb-assert-buffer-in-window)
          (select-window reb-target-window)
          (if query
              (progn
                ;; The preview highlights all matches from the start;
                ;; start replacing there too (a region still limits it).
                (goto-char (or (car bounds) (point-min)))
                ;; `perform-replace' silently switches to case-sensitive
                ;; matching when the regexp contains upper-case letters
                ;; (`search-upper-case'), but the preview does not.
                ;; Bind it to nil so the replacement always follows the
                ;; preview, i.e. the target buffer's `case-fold-search'
                ;; (toggle with `reb-toggle-case').
                (let ((search-upper-case nil))
                  (perform-replace from to t t nil)))
            (rx-query-replace--replace-all from to bounds))
          (rx-query-replace-quit))
      (quit nil))))

(defun rx-query-replace--replace-all (from to bounds)
  "Replace every match of FROM with TO in the current buffer.
Respects BOUNDS (a cons of region limits) when non-nil, and the
buffer's `case-fold-search'.  Returns the replacement count."
  (let ((count 0))
    (save-excursion
      (goto-char (or (car bounds) (point-min)))
      (let ((case-fold-search case-fold-search)
            (nocasify (not (and case-replace case-fold-search))))
        (while (and (not (eobp))
                    (re-search-forward from (or (cdr bounds) (point-max)) t))
          (when (and (= (match-beginning 0) (match-end 0))
                     (not (eobp)))
            (forward-char 1))
          (let ((repl (match-substitute-replacement to nocasify nil)))
            (replace-match repl t t)
            (setq count (1+ count))))))
    (message "Replaced %d occurrence%s" count (if (= count 1) "" "s"))
    count))

(defun rx-query-replace-submit ()
  "Replace using the rx form in the RE Builder buffer.
Reads a replacement string with a live preview of every match,
then runs the replacement in the target buffer.  In
`rx-query-replace' sessions, every match is confirmed with
`perform-replace'; in `rx-replace' sessions, all matches are
replaced at once.  The RE Builder window is closed when the
replacement is done; \\[keyboard-quit] while entering the
replacement aborts and keeps the RE Builder open."
  (interactive)
  (condition-case err
      (rx-query-replace--perform (not rx-query-replace--replace-all))
    (error (message "Invalid rx: %s" (error-message-string err)))))

(defun rx-query-replace-submit-all ()
  "Replace all matches of the rx form at once.
Like `rx-query-replace-submit', but never asks for confirmation
on individual matches."
  (interactive)
  (condition-case err
      (rx-query-replace--perform nil)
    (error (message "Invalid rx: %s" (error-message-string err)))))

(defun rx-query-replace-quit ()
  "Quit `rx-query-replace'.
Restores the previous RE Builder syntax, deletes the overlays,
buries the RE Builder and restores the window configuration."
  (interactive)
  (when (buffer-live-p (get-buffer reb-buffer))
    (with-current-buffer (get-buffer reb-buffer)
      (rx-query-replace-minor-mode -1)
      (reb-quit)))
  (when rx-query-replace--prev-syntax
    (setq reb-re-syntax rx-query-replace--prev-syntax
          rx-query-replace--prev-syntax nil)))

(defun rx-query-replace--enter (replace-all)
  "Enter the rx editor for interactive replacement.
The current buffer becomes the replacement target.  If
REPLACE-ALL is non-nil, `rx-query-replace-submit' replaces all
matches at once instead of querying."
  (setq rx-query-replace--prev-syntax reb-re-syntax)
  (if (and (string= (buffer-name) reb-buffer)
           (reb-mode-buffer-p))
      ;; Already inside the RE Builder: keep its content, just make
      ;; sure the syntax is `rx' and (re)activate the minor mode.
      (progn
        (unless (eq reb-re-syntax 'rx)
          (reb-change-syntax 'rx))
        (setq-local rx-query-replace--replace-all replace-all)
        (rx-query-replace-minor-mode 1))
    (setq reb-re-syntax 'rx)
    (re-builder)
    (with-current-buffer (get-buffer reb-buffer)
      (rx-query-replace--ensure-default)
      (setq-local rx-query-replace--replace-all replace-all)
      (rx-query-replace-minor-mode 1))))

;;;###autoload
(defun rx-query-replace ()
  "Interactively construct an rx regexp and query-replace with it.
Makes the current buffer the \"target\" buffer and displays the
RE Builder buffer with `rx' syntax in another window.  As you edit
the rx form there, matches are highlighted in the target buffer.
Type \\[rx-query-replace-submit] to enter a replacement with a
live preview and then confirm every match,
\\[rx-query-replace-submit-all] to replace all matches at once,
or \\[rx-query-replace-quit] to quit.  See also `rx-replace'."
  (interactive)
  (rx-query-replace--enter nil))

;;;###autoload
(defun rx-replace ()
  "Interactively construct an rx regexp and replace its matches.
Like `rx-query-replace', but \\[rx-query-replace-submit] replaces
all matches at once, without confirming every match."
  (interactive)
  (rx-query-replace--enter t))

(defvar rx-query-replace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'rx-query-replace-submit)
    (define-key map (kbd "C-c C-r") #'rx-query-replace-submit-all)
    (define-key map (kbd "C-c C-k") #'rx-query-replace-quit)
    ;; `reb-mode-map' binds `C-c C-c' to `reb-toggle-case'; our minor
    ;; mode overrides it, so offer the toggle on another key.
    (define-key map (kbd "C-c C-t") #'reb-toggle-case)
    map)
  "Keymap for `rx-query-replace-minor-mode'.")

(define-minor-mode rx-query-replace-minor-mode
  "Minor mode for `rx-query-replace', on top of the RE Builder.
Makes \\[rx-query-replace-submit] run the replacement for the rx
form in the buffer, and \\[rx-query-replace-quit] quit."
  :lighter " rx-qr"
  :keymap rx-query-replace-mode-map)

(provide 'rx-query-replace)

;;; rx-query-replace.el ends here
