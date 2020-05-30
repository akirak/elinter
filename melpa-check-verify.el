;;; melpa-check-verify.el --- Check package configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Version: 0.2
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

;; This library provides a function for checking package configuration
;; of melpa-check.
;;
;; It is implemented to run without the rest of the package to allow
;; running in batch mode.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'lisp-mnt)
(require 'json)

(defgroup melpa-check-verify nil
  "Verify package configuration for melpa-check."
  :group 'melpa-check)

(defvar melpa-check-verify-package-config-errors nil
  "List of errors in the session..")

(defun melpa-check-verify--check-package-config (package)
  "Check the configuration for PACKAGE.

PACKAGE should be an alist, parsed from a JSON string converted
from a Dhall file."
  (let* ((pname (alist-get 'pname package))
         (version (alist-get 'version package))
         (emacsVersion (alist-get 'emacsVersion package))
         (files (alist-get 'files package))
         (dependencies (alist-get 'dependencies package))
         (localDependencies (alist-get 'localDependencies package))
         (mainFile (alist-get 'mainFile package))
         (buttercupTests (alist-get 'buttercupTests package))
         (raw-recipe (alist-get 'recipe package))
         (recipe (read raw-recipe)))
    (cl-labels
        ;; Add information to the error list with descriptive metadata.
        ((add-error (msg &rest objs)
                    (push (format-message "In package %s: %s" pname
                                          (apply #'format-message msg objs))
                          melpa-check-verify-package-config-errors)))
      (condition-case err
          (progn
            (cl-check-type pname string)
            (cl-check-type version string)
            (cl-check-type emacsVersion string)
            (cl-check-type files list)
            (cl-check-type dependencies list)
            (cl-check-type localDependencies list)
            (cl-check-type raw-recipe string)
            (cl-check-type buttercupTests list)
            (cl-check-type mainFile (or string null))
            ;; Check package headers.
            ;;
            ;; If mainFile is explicitly set, only the file will be
            ;; checked.
            (dolist (file (if mainFile
                              (list mainFile)
                            files))
              (with-current-buffer (find-file-noselect file)
                (let* ((file-version (lm-version))
                       (file-raw-dependencies (lm-header-multiline "Package-Requires"))
                       (file-dependencies (when file-raw-dependencies
                                            (read file-raw-dependencies)))
                       (file-emacs-version (car-safe (alist-get 'emacs file-dependencies)))
                       (file-ext-dependencies (mapcar #'symbol-name
                                                      (cl-remove-if (lambda (name)
                                                                      (memq name '(emacs org)))
                                                                    (mapcar #'car file-dependencies))))
                       (missing-packages (cl-set-difference file-ext-dependencies
                                                            dependencies
                                                            :test #'equal)))
                  ;; Check the version of the package itself
                  (when (and file-version
                             (not (equal version file-version)))
                    (add-error "Package version in the header does not match:
  \"%s\" in file %s
  \"%s\" in package %s" file-version file version pname))
                  ;; Check the minimum version of Emacs
                  (unless (equal file-emacs-version
                                 emacsVersion)
                    (add-error "Minimum Emacs version in the header does not match:
  \"%s\" in file %s
  \"%s\" in package %s" file-emacs-version file emacsVersion pname))
                  ;; Check dependencies
                  (when missing-packages
                    (add-error "Missing dependencies:\n  %s (found in file %s but not in the package)"
                               (string-join missing-packages " ")
                               file)))))
            ;; Check if the package has recipe
            (unless recipe
              (add-error "Recipe is empty"))
            ;; Check if the package name in the recipe matches
            (unless (equal pname (symbol-name (car recipe)))
              (add-error "The package name in the recipe does not match pname:
  \"%s\" in the recipe
  \"%s\" pname" (symbol-name (car recipe)) pname)))
        (error (add-error err))))))

(defun melpa-check-verify-package-json-batch ()
  "Check packages passed from the command line.

This function is intended for use in batch mode.

See default.nix in this repository for an example."
  ;; An error can be thrown during the process, so it should be
  ;; handled.
  (condition-case err
      (dolist (json-file command-line-args-left)
        ;; All the remaining command line arguments should be JSON files
        (unless (file-exists-p json-file)
          (error "File does not exist: %s" json-file))
        ;; Parse all the files and verify their data
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (packages (with-temp-buffer
                           (insert-file-contents json-file)
                           (goto-char (point-min))
                           (json-read))))
          (mapc #'melpa-check-verify--check-package-config packages)))
    (error (progn
             (message "Error while verifying the package configuration: %s" err)
             (kill-emacs 1))))
  ;; Abnormal exit if errors are found
  (when melpa-check-verify-package-config-errors
    (message "Errors in the package configuration:\n%s"
             (string-join melpa-check-verify-package-config-errors "\n\n"))
    (kill-emacs 1)))

(provide 'melpa-check-verify)
;;; melpa-check-verify.el ends here

