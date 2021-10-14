;;; elinter-lint.el --- Run multiple elisp linters in batch -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (package-lint "0") (package-build "0"))
;; Keywords: lisp maint
;; URL: https://github.com/akirak/elinter

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

;; 

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'package-build)
(require 'package-lint)

(defvar package-lint-main-file)

(defgroup elinter nil
  "Lint runner for Emacs Lisp projects."
  :prefix "elinter"
  :group 'maint
  :group 'lisp)

(defvar elinter-recipe-dir nil)

(defcustom elinter-prebuild-dir ".elinter-prebuild"
  ""
  :type 'directory)

(defvar elinter-package-elisp-files
  (split-string (or (getenv "PACKAGE_ELISP_FILES") "") " ")
  "Input files.")

(defvar elinter-package-main-file (getenv "PACKAGE_MAIN_FILE")
  "Main file of the package.")

(defcustom elinter-enabled-linters
  (cl-labels
      ((env-as-list (name) (let ((val (getenv name)))
                             (and val (split-string val " ")))))
    (or (env-as-list "ELINTER_LINTERS")
        '("package-lint"
          "checkdoc"
          "check-declare")))
  "List of linters to run."
  :type '(repeat string))

(defcustom elinter-allow-warnings
  (cl-labels
      ((env-as-list (name) (let ((val (getenv name)))
                             (and val
                                  (if (member val '("1" "true"))
                                      t
                                    (split-string val " "))))))
    (env-as-list "ELINTER_ALLOW_WARNINGS"))
  "Whether to successfully exit when only warnings occurrs.

This variable can also be a list of linter names."
  :type '(choice boolean
                 (repeat string)))

(defvar elinter-failed-linters nil
  "List of linters that have failed.")

(defvar elinter-warning-linters nil
  "List of linters that produced warnings.")

(defun elinter-package-lint ()
  "Run package-lint on the input files."
  ;; Prevent use of package.el to install packages
  (advice-add #'package-initialize :override #'ignore)
  (advice-add #'package-lint--check-packages-installable :override #'ignore)

  (setq package-lint-main-file elinter-package-main-file)
  (unless (package-lint-batch-and-exit-1 elinter-package-elisp-files)
    ;; TODO: Distinguish between error and warning
    (throw 'failure '((errors . t)))))

(defvar elinter-checkdoc-found-errors)

(defun elinter-checkdoc-on-file (file)
  "Run checkdoc on FILE in batch mode."
  (condition-case err
      (let ((checkdoc-create-error-function
             (lambda (text start end &optional unfixable)
               (setq elinter-checkdoc-found-errors t)
               (let ((start-line (count-lines (point-min) (or start (point-min))))
                     (col (car (posn-col-row (posn-at-point (or start (point-min)))))))
                 (message (format-message
                           "%s:%d:%d: error: %s%s"
                           (checkdoc-buffer-label)
                           start-line col
                           text
                           (if start
                               (save-excursion
                                 (goto-char start)
                                 (let ((func (which-function)))
                                   (if func
                                       (format " (in %s)" func)
                                     "")))
                             ""))))
               (list text start end unfixable))))
        (checkdoc-file (file-truename file)))
    (error (unless (eq 'user-error (car err))
             (message "%s" err)
             (message "Checkdoc failed on %s" file)
             (setq elinter-checkdoc-found-errors t)))))

;; Based on the implementation of makem.sh by alphapapa
;; https://github.com/alphapapa/makem.sh/blob/master/makem.sh
(defun elinter-checkdoc ()
  "Run checkdoc in batch mode."
  (cond
   ((version< emacs-version "25")
    (message "warning: Due to API incompatibility, checkdoc isn't supported on Emacs 24.x")
    (throw 'failure 'warning))
   ((version< emacs-version "27")
    (message "warning: Due to some reasons, checkdoc is run only on Emacs 27 and later"))
   (t
    (require 'checkdoc)
    (require 'which-func)
    (setq elinter-checkdoc-found-errors nil)
    (unless (boundp 'checkdoc-create-error-function)
      (message "warning: Checkdoc version looks old. Recommend updating")
      (defvar checkdoc-create-error-function nil)
      (advice-add #'checkdoc-create-error
                  :override
                  (lambda (text start end &optional unfixable)
                    (funcall checkdoc-create-error-function text start end unfixable))))
    (mapc #'elinter-checkdoc-on-file elinter-package-elisp-files)
    (when elinter-checkdoc-found-errors
      (throw 'failure '((errors . t)))))))

(defun elinter-check-declare ()
  "Run `check-declare' on the input files."
  (require 'check-declare)
  (require 'warnings)
  (let (has-errors
        (initial-warning-minimum-log-level warning-minimum-log-level))
    (setq warning-minimum-log-level :emergency)
    (unwind-protect
        (dolist (file elinter-package-elisp-files)
          (let* ((truename (file-truename file))
                 (default-directory (file-name-directory truename))
                 (errors (check-declare-files
                          (file-name-nondirectory truename)))
                 (messages (cl-loop for (fnfile . errs) in errors
                                    append
                                    (cl-loop for (file fn msg) in errs
                                             collect
                                             (format-message
                                              ;; TODO: Determine whether it is an error or warning
                                              "%s: error: said `%s' was defined in %s: %s"
                                              file
                                              fn
                                              fnfile
                                              msg)))))
            (when messages
              (setq has-errors t)
              (message (string-join messages "\n")))))
      (setq warning-minimum-log-level initial-warning-minimum-log-level))
    (when has-errors
      (throw 'failure '((errors . t))))))

(defun elinter-melpazoid ()
  "Run extra checks from melpazoid on the input files."
  (let ((noninteractive nil))
    (require 'melpazoid)
    (setq melpazoid--misc-header-printed-p t)
    (let (has-errors)
      (dolist (file elinter-package-elisp-files)
        (ignore-errors
          (kill-buffer melpazoid-buffer))
        (let ((file-buf (create-file-buffer file)))
          (with-current-buffer file-buf
            (setq buffer-file-name file)
            (insert-file-contents file)
            (melpazoid-check-sharp-quotes)
            (melpazoid-check-misc))
          (let ((err-buf (get-buffer melpazoid-buffer)))
            (when (and err-buf
                       (buffer-live-p err-buf)
                       (> (buffer-size err-buf) 0))
              (setq has-errors t)
              (with-current-buffer err-buf
                (message (string-trim (buffer-string))))))))
      ;; TODO: Distinguish between error and warning
      (when has-errors
        (throw 'failure '((warnings . t)))))))

(defvar elinter-continuing nil)

(defun elinter-run-linter (linter)
  "Run a LINTER by name."
  (let ((func (intern (concat "elinter-" linter))))
    ;; Insert an empty line
    (if elinter-continuing
        (message "")
      (setq elinter-continuing t))
    (condition-case err
        (progn
          (message "Running %s..." linter)
          (unless (fboundp func)
            (error "Function not found: %s" func))
          (let* ((result (catch 'failure
                           (funcall func)
                           (message "SUCCESS")
                           nil))
                 (errors (cdr (assoc 'errors result)))
                 (warnings (cdr (assoc 'warnings result))))
            (cond
             ((and warnings
                   (not errors)
                   (or (eql t elinter-allow-warnings)
                       (and (listp elinter-allow-warnings)
                            (member linter elinter-allow-warnings))))
              (message "WARN: Found warnings, but exit successfully")
              (push linter elinter-warning-linters))
             ((or errors
                  warnings)
              (message "FAILED")
              (push linter elinter-failed-linters)))))
      (error
       (progn
         (message "FAILED: Unexpected error from %s: %s" linter err)
         (push linter elinter-failed-linters))))))

(defun elinter-run-linters-current-package ()
  "Run the linters on the package configured in the variables.

This function returns non-nil if there is any error found."
  (setq elinter-failed-linters nil)
  (mapc #'elinter-run-linter elinter-enabled-linters)
  (when elinter-failed-linters
    (message "\nThe following checks have failed: %s"
             (string-join (nreverse elinter-failed-linters) " ")))
  (when elinter-warning-linters
    (message "\nThe following checks raised warnings: %s"
             (string-join (nreverse elinter-warning-linters) " ")))
  elinter-failed-linters)

(defvar package-build-default-files-spec)

(defun elinter-read-recipes ()
  "Read all recipes from `elinter-recipe-dir'."
  (thread-last (directory-files elinter-recipe-dir t)
    (cl-remove-if (lambda (file)
                    (string-match-p (rx "/." (* (not (any "/"))) eol) file)))
    (mapcar (lambda (file)
              (ignore-errors
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (read (current-buffer))))))))

(defun elinter--main-file (package-name source-files)
  "Find the main file for PACKAGE-NAME from SOURCE-FILES."
  (cl-find-if
   (lambda (file)
     (string-equal (file-name-nondirectory file) (concat package-name ".el")))
   source-files))

(defun elinter-lint-on-files (files)
  "Run the linters on FILES based on the in-repository recipes."
  (cl-flet
      ((expand-recipe-files (recipe)
                            (let ((files (plist-get (cdr recipe) :files)))
                              (mapcar #'car
                                      (package-build-expand-file-specs
                                       default-directory
                                       (if (eq :defaults (car files))
                                           (append package-build-default-files-spec
                                                   (cdr files))
                                         (or files
                                             package-build-default-files-spec)))))))
    (let* (failure
           (recipes (elinter-read-recipes)))
      (dolist (recipe recipes)
        (when recipe
          (let* ((package-name (symbol-name (car recipe)))
                 (package-files (expand-recipe-files recipe))
                 (matches (cl-intersection package-files files :test #'file-equal-p)))
            (when matches
              (message "Linting files in package %s: %s"
                       package-name
                       (string-join matches " "))
              (setq elinter-package-elisp-files matches
                    elinter-package-main-file (elinter--main-file package-name package-files))
              (when (elinter-run-linters-current-package)
                (setq failure t))))))
      failure)))

(defun elinter-lint-and-prepare-compile ()
  ""
  (unless (file-directory-p elinter-prebuild-dir)
    (make-directory elinter-prebuild-dir t))
  (let (failure)
    (dolist (recipe (elinter-read-recipes))
      (let* ((package-name (symbol-name (car recipe)))
             (files-spec (plist-get (cdr recipe) :files))
             (package-files (package-build-expand-file-specs
                             default-directory
                             (if (eq :defaults (car files-spec))
                                 (append package-build-default-files-spec
                                         (cdr files-spec))
                               (or files-spec
                                   package-build-default-files-spec))))
             (elisp-files (cl-remove-if (lambda (fpath)
                                          (string-match-p (rx ".el" bol) fpath))
                                        (mapcar #'car package-files)))
             (prebuild-dest (expand-file-name package-name elinter-prebuild-dir))
             (main-file (elinter--main-file package-name elisp-files)))
        (message "Linting files in package %s: %s"
                 package-name
                 (string-join elisp-files " "))
        (setq elinter-package-elisp-files elisp-files
              elinter-package-main-file main-file)
        (when (elinter-run-linters-current-package)
          (setq failure t))
        (when (file-directory-p prebuild-dest)
          (delete-directory prebuild-dest 'recursive))
        (make-directory prebuild-dest)
        (pcase-dolist (`(,src . ,name) package-files)
          (make-symbolic-link (expand-file-name src default-directory)
                              (expand-file-name name prebuild-dest)))
        (with-temp-buffer
          (prin1 (elinter--define-package main-file)
                 (current-buffer))
          (write-file (expand-file-name (concat package-name "-pkg.el")
                                        prebuild-dest)))))
    failure))

(require 'lisp-mnt)

(defun elinter--define-package (main-file)
  (cl-check-type main-file string)
  (with-temp-buffer
    (insert-file-contents main-file)
    `(define-package ,(lm-get-package-name)
       ,(lm-version)
       "Omitted. Use package-build.el for retrieving a proper value."
       ,(let ((lines (lm-header-multiline "package-requires")))
          (with-temp-buffer
            (insert (string-join lines " "))
            (goto-char (point-min))
            `',(read (current-buffer)))))))

(defun elinter-lint-run-and-exit ()
  "Run the linters and kill Emacs with an appropriate exit code."
  (let ((failure (if command-line-args-left
                     (elinter-lint-on-files command-line-args-left)
                   (elinter-lint-and-prepare-compile))))
    (kill-emacs (if failure 1 0))))

(defun elinter-setup-from-env ()
  "Configure linters from the environment."
  (let ((custom-file (getenv "ELINTER_LINT_CUSTOM_FILE")))
    (when (and custom-file
               (file-exists-p custom-file))
      (load custom-file nil 'nomessage)))
  (let ((recipe-dir (getenv "ELINTER_RECIPE_DIR")))
    (when (and recipe-dir
               (file-directory-p recipe-dir))
      (setq elinter-recipe-dir (file-name-as-directory recipe-dir))))
  (let ((prebuild-dir (getenv "ELINTER_PREBUILD_DIR")))
    (when prebuild-dir
      (setq elinter-prebuild-dir (file-name-as-directory prebuild-dir)))))

(when noninteractive
  (elinter-setup-from-env)
  (elinter-lint-run-and-exit))

(provide 'elinter-lint)
;;; elinter-lint.el ends here
