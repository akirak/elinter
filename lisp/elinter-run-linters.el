;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'subr-x)

(defgroup elinter nil
  "Lint runner for Emacs Lisp projects.")

(defvar elinter-package-elisp-files
  (split-string (getenv "PACKAGE_ELISP_FILES") " ")
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

(defvar elinter-lint-failures nil
  "List of linters that have failed.")

(defun elinter-package-lint ()
  "Run package-lint on the input files."
  (require 'package-lint)

  ;; Prevent use of package.el to install packages
  (advice-add #'package-initialize :override #'ignore)
  (advice-add #'package-lint--check-packages-installable :override #'ignore)

  (setq package-lint-main-file elinter-package-main-file)
  (unless (package-lint-batch-and-exit-1 elinter-package-elisp-files)
    (throw 'failure t)))

(defvar elinter-checkdoc-found-errors)

(defun elinter-checkdoc-on-file (file)
  "Run checkdoc on FILE in batch mode."
  (condition-case err
      (let ((checkdoc-create-error-function
             (lambda (text start end &optional unfixable)
               (setq elinter-checkdoc-found-errors t)
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
        (checkdoc-file (file-truename file)))
    (error (progn
             (message err)
             (message "Checkdoc failed on %s" file)
             (setq elinter-checkdoc-found-errors t)))))

;; Based on the implementation of makem.sh by alphapapa
;; https://github.com/alphapapa/makem.sh/blob/master/makem.sh
(defun elinter-checkdoc ()
  "Run checkdoc in batch mode."
  (require 'checkdoc)
  (setq elinter-checkdoc-found-errors nil)
  (unless (boundp 'checkdoc-create-error-function)
    (message "Warning: Checkdoc version looks old. Recommend updating")
    (defvar checkdoc-create-error-function nil)
    (advice-add #'checkdoc-create-error
                :override
                (lambda (text start end &optional unfixable)
                  (funcall checkdoc-create-error-function text start end unfixable))))
  (mapc #'elinter-checkdoc-on-file elinter-package-elisp-files)
  (when elinter-checkdoc-found-errors
    (throw 'failure t)))

(defun elinter-check-declare ()
  "Run `check-declare' on the input files."
  ;; Based on an equivalent function from makem.sh by alphapapa.
  ;; <https://github.com/alphapapa/makem.sh/blob/master/makem.sh>
  (require 'check-declare)
  ;; `check-declare-files' returns a list of errors. It would be
  ;; possible to retrieve information on the errors from the variable
  ;; rather than opening the file.
  (when (apply #'check-declare-files elinter-package-elisp-files)
    (with-current-buffer check-declare-warning-buffer
      (message (buffer-string))
      (throw 'failure t))))

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
      (when has-errors
        (throw 'failure t)))))

(defvar elinter-continueing nil)

(defun elinter-run-linter (linter)
  "Run a LINTER by name."
  (let ((func (intern (concat "elinter-" linter))))
    ;; Insert an empty line
    (if elinter-continueing
        (message "")
      (setq elinter-continueing t))
    (condition-case err
        (progn
          (message "Running %s..." linter)
          (unless (fboundp func)
            (error "Function not found: %s" func))
          (when (catch 'failure
                  (funcall func)
                  (message "SUCCESS")
                  nil)
            (message "FAILED")
            (push linter elinter-lint-failures)))
      (error
       (progn
         (message "FAILED: Unexpected error from %s: %s" linter err)
         (push linter elinter-lint-failures))))))

(defun elinter-run-linters-and-exit ()
  "Run the linters and kill Emacs with an appropriate exit code."
  (setq elinter-lint-failures nil)
  (mapc #'elinter-run-linter elinter-enabled-linters)
  (when elinter-lint-failures
    (message "\nThe following checks have failed: %s"
             (string-join (nreverse elinter-lint-failures) " ")))
  (kill-emacs (if elinter-lint-failures 1 0)))

(when noninteractive
  (elinter-run-linters-and-exit))

;; Local Variables:
;; mode: emacs-lisp
;; End:
