;; -*- lexical-binding: t -*-

;; Based on the implementation of makem.sh by alphapapa
;; https://github.com/alphapapa/makem.sh/blob/master/makem.sh

(require 'checkdoc)
(require 'which-func)
(require 'subr-x)

(defvar checkdoc-version-too-old nil)
(unless (boundp 'checkdoc-create-error-function)
  (message "Checkdoc version is too old.
Use a version that supports `checkdoc-create-error-function'.
Continuing anyway")
  (setq checkdoc-version-too-old t))

(defvar checkdoc-error-targets nil)

;; To run spellcheck, you have to configure a program for spell
;; checking.
;;
;; TODO: Configure `ispell-program-name'
;;
;; (setq checkdoc-spellcheck-documentation-flag t)

(defun batch-checkdoc-file (file)
  "Run checkdoc on FILE in batch mode."
  (message "----------------------------------------------------------")
  (message "Running checkdoc on %s..." file)
  (condition-case err
      (let ((checkdoc-create-error-function
             (lambda (text start end &optional unfixable)
               (add-to-list 'checkdoc-error-targets file t)
               (let* ((start-line (count-lines (point-min) (or start (point-min))))
                      (end-line (count-lines (point-min) (or end (point-min))))
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
             (add-to-list 'checkdoc-error-targets file t))))
  (unless (member file checkdoc-error-targets)
    (message "No errors found.")))

(message "Checkdoc version: %s" checkdoc-version)

(mapc #'batch-checkdoc-file command-line-args-left)

(message "----------------------------------------------------------")

(cond
 (checkdoc-error-targets
  (dolist (file checkdoc-error-targets)
    (message "%s has checkdoc errors" file))
  (kill-emacs 1))
 (checkdoc-version-too-old
  (message "Use a newer version of checkdoc.")
  (kill-emacs 2))
 (t
  (message "Successfully ran checkdoc on all files")
  (kill-emacs)))
