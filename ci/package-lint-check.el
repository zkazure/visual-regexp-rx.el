;;; package-lint-check.el --- CI helper: run package-lint on the package

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

;; Runs package-lint on visual-regexp-rx.el and exits non-zero on
;; issues.  vr/engine is a shared defcustom with visual-regexp-steroids
;; (like steroids' own vr/ symbols), so it is whitelisted explicitly.

;;; Code:

(require 'package-lint)

(setq package-lint--allowed-prefix-mappings
      (cons '("visual-regexp-rx" . ("vr/"))
            package-lint--allowed-prefix-mappings))
(setq package-lint--sane-prefixes
      (concat "\\`\\(?:vr/\\|"
              (substring package-lint--sane-prefixes 6)))

(find-file "visual-regexp-rx.el")
(let ((issues (package-lint-buffer)))
  (if issues
      (progn
        (dolist (i issues)
          (princ (format "%d:%d: %s: %s\n"
                         (nth 0 i) (nth 1 i) (nth 2 i) (nth 3 i))))
        (kill-emacs 1))
    (princ "package-lint: clean\n")))

;;; package-lint-check.el ends here
