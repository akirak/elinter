;;; melpa-check-multisel.el --- Library for multiple selection -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Version: 0.1
;; Author: Akira Komamura <akira.komamura@gmail.com>
;; URL: https://github.com/akirak/melpa-check

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a multi-backend function for selecting
;; multiple items from a list of strings.

;;; Code:

(require 'cl-lib)

(declare-function helm "ext:helm")
(declare-function helm-marked-candidates "ext:helm")
(declare-function helm-build-sync-source "ext:helm")
(declare-function helm-marked-candidates "ext:helm")

(defgroup melpa-check-multisel nil
  "Muliple selection for melpa-check"
  :group 'melpa-check
  :prefix "melpa-check")

(defcustom melpa-check-multisel-backend
  (cond
   ((require 'helm nil t)
    'helm)
   (t
    'read-sexp))
  "Library used to select multiple strings."
  :type '(choice (const :tag "Helm (requires helm.el)" helm)
                 (const :tag "Builtin read-sexp function" read-sexp)))

(defun melpa-check-multisel (prompt candidates)
  "With PROMPT, select items from CANDIDATES."
  (cl-ecase melpa-check-multisel-backend
    (helm (progn
            (require 'helm)
            (helm :prompt prompt
                  :sources
                  (list (helm-build-sync-source "Select items"
                          :candidates candidates
                          :action (lambda (c)
                                    (or (helm-marked-candidates)
                                        c)))
                        (helm-build-sync-source "Commands"
                          :candidates '("Quit selection")
                          :action (lambda (_) nil))))))
    (read-sexp (read--expression prompt
                                 (prin1-to-string candidates)))))

(provide 'melpa-check-multisel)
;;; melpa-check-multisel.el ends here
