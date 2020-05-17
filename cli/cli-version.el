;;; cli-version.el --- Update and check the version number -*- lexical-binding: t -*-

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

;; This library provides functions for checking and updating the
;; version of the command line interface of melpa-check.
;;
;; To use this library, call either `cli-version-update' or
;; `cli-version-check' in batch mode from the command line or the
;; shell script.

;;; Code:

(require 'lisp-mnt)

(defconst cli-version-elisp-source-file "../melpa-check.el")

(defconst cli-version-target-file "src/Version.purs")

(defun cli-version-update ()
  "Update the version of the CLI."
  (cli-version-check t))

(defun cli-version-check (&optional update)
  "Check the version of the CLI.

If UPDATE is non-nil, update the version of the CLI rather than
check it."
  (with-current-buffer (create-file-buffer cli-version-target-file)
    (insert-file-contents cli-version-target-file)
    (goto-char (point-min))
    (re-search-forward (rx "versionString = "))
    (let ((cli-version (let ((str (buffer-substring (point) (line-end-position))))
                         (if (string-match (rx "\"" (group (+ digit) (optional "." (+ digit))) "\"")
                                           str)
                             (match-string 1 str)
                           (error "Did not match a version number in %s" str))))
          (elisp-version (cli-version--get-elisp-version)))
      (if update
          (progn
            (delete-region (point) (line-end-position))
            (insert (format "\"%s\"" elisp-version))
            (setq buffer-file-name cli-version-target-file)
            (set (make-local-variable 'backup-inhibited) t)
            (save-buffer))
        (message "CLI version:   %s in (%s)" cli-version cli-version-target-file)
        (message "Elisp version: %s in (%s)" elisp-version cli-version-elisp-source-file)
        (if (string-equal elisp-version cli-version)
            (message "OK. The versions match")
          (message "Did not match")
          (kill-emacs 1))))))

(defun cli-version--get-elisp-version ()
  "Get the version of the Emacs Lisp package."
  (with-temp-buffer
    (insert-file-contents cli-version-elisp-source-file)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (lm-header "Version")))

(provide 'cli-version)
;;; cli-version.el ends here
