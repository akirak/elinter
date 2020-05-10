;;; checkdoc-runner.el --- Run checkdoc inside Nix -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: maint
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

;; This is a script for running checkdoc inside a Nix session.

;;; Code:

;; Based on the implementation of makem.sh by alphapapa
;; https://github.com/alphapapa/makem.sh/blob/master/makem.sh

(require 'checkdoc)
(require 'which-func)
(require 'subr-x)

(require 'find-func nil t)
(require 'lisp-mnt nil t)

(defun checkdoc-runner--checkdoc-version ()
  "Get the version of checkdoc."
  ;; These functions are unavailable in relative old versions of
  ;; Emacs.
  (if (and (fboundp #'find-library-name)
           (fboundp #'lm-header))
      (with-current-buffer (find-file-noselect
                            (find-library-name "checkdoc"))
        (lm-header "Version"))
    ;; This variable is obsolete as of Emacs 28.1.
    ;;
    ;; To silence a byte-compilation error, I will wrap it in
    ;; `bound-and-true-p'.
    (bound-and-true-p checkdoc-version)))

(defvar checkdoc-runner-version-too-old nil)
(unless (boundp 'checkdoc-create-error-function)
  (message "Checkdoc version is too old.
Use a version that supports `checkdoc-create-error-function'.
Continuing anyway")
  (setq checkdoc-runner-version-too-old t))

(defvar checkdoc-runner-error-targets nil)

;; To run spellcheck, you have to configure a program for spell
;; checking.
;;
;; TODO: Configure `ispell-program-name'
;;
;; (setq checkdoc-spellcheck-documentation-flag t)

(defun checkdoc-runner-batch-file (file)
  "Run checkdoc on FILE in batch mode."
  (message "----------------------------------------------------------")
  (message "Running checkdoc on %s..." file)
  (condition-case err
      (let ((checkdoc-create-error-function
             (lambda (text start end &optional unfixable)
               (add-to-list 'checkdoc-runner-error-targets file t)
               (let* ((start-line (count-lines (point-min) (or start (point-min))))
                      (msg (concat "\n" (checkdoc-buffer-label)
                                   ":"
                                   (int-to-string start-line)
                                   ":"
                                   (if start
                                       (save-excursion
                                         (goto-char start)
                                         (or (which-function) ""))
                                     "")
                                   "\n  "
                                   text
                                   (if (and start end (> end start)
                                            (not (string-equal text
                                                               "All interactive functions should have documentation")))
                                       (concat "\n  > " (buffer-substring start end))
                                     ""))))
                 (message msg))
               (list text start end unfixable))))
        (checkdoc-file file))
    (error (progn
             (message err)
             (message "Checkdoc failed on %s" file)
             (add-to-list 'checkdoc-runner-error-targets file t))))
  (unless (member file checkdoc-runner-error-targets)
    (message "No errors found.")))

(message "Checkdoc version: %s" (checkdoc-runner--checkdoc-version))

(mapc #'checkdoc-runner-batch-file command-line-args-left)

(message "----------------------------------------------------------")

(cond
 (checkdoc-runner-error-targets
  (dolist (file checkdoc-runner-error-targets)
    (message "%s has checkdoc errors" file))
  (kill-emacs 1))
 (checkdoc-runner-version-too-old
  (message "Use a newer version of checkdoc.")
  (kill-emacs 2))
 (t
  (message "Successfully ran checkdoc on all files")
  (kill-emacs)))

(provide 'checkdoc-runner)
;;; checkdoc-runner.el ends here

