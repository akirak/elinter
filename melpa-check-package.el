;;; melpa-check-package.el --- Building package configuration -*- lexical-binding: t -*-

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

;; This library provides functions for building package configuration
;; for melpa-check.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'lisp-mnt)

(require 'melpa-check-git)

(defgroup melpa-check-package nil
  "Build package configuration."
  :group 'melpa-check)

(defcustom melpa-check-package-default-excludes-spec
  '(".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el")
  "Default value :exclude spec.

See `package-build-default-files-spec' for an authentic value."
  :type '(repeat string))

(cl-defstruct melpa-check-package
  pname version emacsVersion files dependencies localDependencies
  testDrivers ertTests buttercupTests mainFile recipe)

(defun melpa-check-package--to-alist (package)
  "Build an alist containing data for PACKAGE.

The result may contain nil values."
  `((pname . ,(melpa-check-package-pname package))
    (version . ,(melpa-check-package-version package))
    (emacsVersion . ,(melpa-check-package-emacsVersion package))
    (files . ,(melpa-check-package-files package))
    (dependencies . ,(melpa-check-package-dependencies package))
    (localDependencies . ,(melpa-check-package-localDependencies package))
    (testDrivers . ,(melpa-check-package-testDrivers package))
    (ertTests . ,(melpa-check-package-ertTests package))
    (buttercupTests . ,(melpa-check-package-buttercupTests package))
    (mainFile . ,(melpa-check-package-mainFile package))
    (recipe . ,(melpa-check-package-recipe package))))

(cl-defun melpa-check-package--from-files (files &key multi-package
                                                 all-files)
  "Build a package data for files.

This function returns data of `melpa-check-package' type
for FILES.

If the repository contains multiple packages, you should set
MULTI-PACKAGE to non-nil and ALL-FILES to all Emacs Lisp
source files, excluding tests.

When you call this function, you have to set `default-directory'
to the root of the project (usually a Git repository)."
  (let* ((obviousMainFile (when (= 1 (length files))
                            (car files)))
         ;; Ask the user for the main file of the package if there are
         ;; more than one files.
         (explicitMainFile (unless obviousMainFile
                             (completing-read "Main file of the package: "
                                              (-sort (lambda (a b)
                                                       (< (length a)
                                                          (length b)))
                                                     files))))
         ;; Build an initial data of the package with only files and
         ;; mainFile.
         (package (make-melpa-check-package
                   :files files
                   :mainFile explicitMainFile))
         ;; Set the main file either from the user input or the source
         ;; file name.
         (mainFile (or explicitMainFile
                       obviousMainFile))
         ;; Set pname from the base name of the main file.
         (pname (read-string "Name of the package: "
                             (file-name-base mainFile)))
         dependencies)
    (setf (melpa-check-package-pname package) pname)
    ;; Set the version and the Emacs version from the header of the
    ;; main file.
    ;;
    ;; If the data is missing in the header, ask the user.
    (with-current-buffer (or (find-buffer-visiting mainFile)
                             (find-file-noselect mainFile))
      (save-restriction
        (widen)
        (save-excursion
          (let* ((version (lm-header "Version"))
                 (deps-raw (lm-header-multiline "Package-Requires"))
                 (emacsVersion (if deps-raw
                                   (car-safe (alist-get 'emacs (read (car deps-raw)))))))
            (setf (melpa-check-package-version package)
                  (or version
                      (read-string "Package version: ")))
            (setf (melpa-check-package-emacsVersion package)
                  (or emacsVersion
                      (read-string "Minimum version of Emacs: ")))))))
    ;; Retrieve dependencies from the headers of all files.
    (dolist (file files)
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (save-restriction
          (widen)
          (save-excursion
            (let* ((deps-raw (lm-header-multiline "Package-Requires")))
              (when deps-raw
                (setq dependencies (append dependencies
                                           (-map #'car (read (car deps-raw)))))))))))
    (setf (melpa-check-package-dependencies package)
          (-map #'symbol-name (-uniq (delq 'emacs dependencies))))
    (setf (melpa-check-package-testDrivers package)
          (cl-case (read-char-choice "Test driver [e: ert, b: buttercup, n: none]: "
                                     (string-to-list "ebn"))
            (?e (list "ert"))
            (?b (list "buttercup"))
            (?n nil)))
    ;; Set recipe
    (let ((fetcher-spec (melpa-check-git--fetcher-spec))
          (files-spec (melpa-check-package--files-spec files
                                                       :multi-package
                                                       multi-package
                                                       :all-files
                                                       all-files)))
      (setf (melpa-check-package-recipe package)
            (prin1-to-string `(,(intern pname)
                               ,@fetcher-spec
                               ,@(when files-spec
                                   `(:files ,files-spec))))))
    package))

(cl-defun melpa-check-package--files-spec (files &key multi-package
                                                 all-files)
  "Build files spec for a package recipe.

See `melpa-check-package
--from-files' on FILES, MULTI-PACKAGE,
and ALL-FILES."
  (let ((dirs (-uniq (-map #'file-name-directory files))))
    (cl-labels
        ((sexp-to-string
          (sexp)
          (prin1-to-string sexp))
         (no-slash
          (dir)
          (string-remove-suffix "/" dir))
         ;; Build a spec with directories and the wildcard file
         ;; pattern.
         (subdir-files
          ()
          (cond
           ;; Multiple directories
           ((> (length dirs) 1)
            `("*.el" (,@(-map #'no-slash dirs))))
           ((or (string-empty-p (car dirs))
                (not (car dirs)))
            '("*.el"))
           ;; Single directory
           (t
            (list (format "%s/*.el" (no-slash (car dirs)))))))
         ;; If some of the source files are included in a directory
         ;; other than the repository root.
         (in-subdir-p
          ()
          (not (equal dirs (list nil))))
         ;; Build the default spec with other source files excluded.
         (defaults-with-excludes
           ()
           `(:defaults
             (:exclude
              ,@(-difference all-files files)
              ,@melpa-check-package-default-excludes-spec))))
      (cond
       ;; Multiple packages
       (multi-package
        ;; Continue without files spec if the input is invalid
        ;; because it would be painful to restart from the beginning
        (ignore-errors
          (read (completing-read
                 "Specify files for the recipe: "
                 (->> (list files
                            (when (in-subdir-p)
                              (subdir-files))
                            (defaults-with-excludes))
                      (delq nil)
                      (mapcar #'sexp-to-string))))))
       ;; Single package in the repository root
       ((and (not multi-package)
             (-all-p #'null dirs))
        ;; Use the default spec (omitted output)
        nil)
       ;; Single package in subdirectories
       ((not multi-package)
        ;; Return nil if it fails to parse, e.g. the input is empty
        (ignore-errors
          (read--expression "Specify files for the recipe: "
                            (sexp-to-string
                             (subdir-files)))))
       ;; This never matches unless there is a bug
       (t
        (error "Unexpected clause"))))))

(provide 'melpa-check-package)
;;; melpa-check-package.el ends here

