;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'subr-x)

(defgroup elinter nil
  "Lint runner for Emacs Lisp projects."
  :group 'maint
  :group 'lisp)

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

(defvar elinter-lint-errors nil
  "List of linters that have failed.")

(defvar elinter-lint-warnings nil
  "List of linters that produced warnings.")

(defun elinter-package-lint ()
  "Run package-lint on the input files."
  (require 'package-lint)

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
    (error (progn
             (message err)
             (message "Checkdoc failed on %s" file)
             (setq elinter-checkdoc-found-errors t)))))

;; Based on the implementation of makem.sh by alphapapa
;; https://github.com/alphapapa/makem.sh/blob/master/makem.sh
(defun elinter-checkdoc ()
  "Run checkdoc in batch mode."
  (if (version< emacs-version "25")
      (progn
        (message "warning: Due to API incompatibility, checkdoc isn't supported on Emacs 24.x")
        (throw 'failure 'warning))
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
      (throw 'failure '((errors . t))))))

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
            (when errors
              (message "FAILED")
              (push linter elinter-lint-errors))
            (when warnings
              (message "WARN: Found warnings, but exit successfully")
              (push linter elinter-lint-warnings))))
      (error
       (progn
         (message "FAILED: Unexpected error from %s: %s" linter err)
         (push linter elinter-lint-errors))))))

(defun elinter-run-linters-current-package ()
  "Run the linters on the package configured in the variables.

This function returns non-nil if there is any error found."
  (setq elinter-lint-errors nil)
  (mapc #'elinter-run-linter elinter-enabled-linters)
  (when elinter-lint-errors
    (message "\nThe following checks have failed: %s"
             (string-join (nreverse elinter-lint-errors) " ")))
  (when elinter-lint-warnings
    (message "\nThe following checks raised warnings: %s"
             (string-join (nreverse elinter-lint-warnings) " ")))
  elinter-lint-errors)

(defvar package-build-default-files-spec)

(defun elinter-run-linters-on-files (files)
  "Run the linters on FILES based on the in-repository recipes."
  (require 'package-build)
  (cl-labels
      ((read-recipe (file)
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (read (current-buffer)))))
       (expand-recipe-files (recipe)
                            (mapcar #'car
                                    (package-build-expand-file-specs
                                     default-directory
                                     (or (plist-get (cdr recipe) :files)
                                         package-build-default-files-spec))))
       (main-file (package-name source-files)
                  (car (cl-find-if
                        (lambda (file)
                          (let ((base (file-name-base file)))
                            (member base (list (concat package-name ".el")
                                               (concat package-name "-pkg.el")))))
                        source-files))))
    (let* (failure
           (recipes (mapcar #'read-recipe (directory-files ".recipes" t))))
      (dolist (recipe recipes)
        (when recipe
          (let* ((package-name (symbol-name (car recipe)))
                 (package-files (expand-recipe-files recipe))
                 (matches (cl-intersection package-files files
                                           :test #'file-equal-p)))
            (when matches
              (message "Linting files in package %s: %s"
                       package-name
                       (string-join matches " "))
              (setq elinter-package-elisp-files matches
                    elinter-package-main-file (main-file package-name package-files))
              (when (elinter-run-linters-current-package)
                (setq failure t))))))
      failure)))

(defun elinter-run-linters-and-exit ()
  "Run the linters and kill Emacs with an appropriate exit code."
  (let ((failure (if command-line-args-left
                     (elinter-run-linters-on-files command-line-args-left)
                   (elinter-run-linters-current-package))))
    (kill-emacs (if failure 1 0))))

(when noninteractive
  (elinter-run-linters-and-exit))

;; Local Variables:
;; mode: emacs-lisp
;; End:
