(defvar melpa-check-package-config-errors nil)

(defun melpa-check--check-package-config (package)
  (require 'subr-x)
  (require 'cl-lib)
  (require 'lisp-mnt)
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
        ((add-error (msg &rest objs)
                    (push (format-message "In package %s: %s" pname
                                          (apply #'format-message msg objs))
                          melpa-check-package-config-errors)))
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
                  (when (and file-version
                             (not (equal version file-version)))
                    (add-error "Package version in the header does not match:
  \"%s\" in file %s
  \"%s\" in package %s" file-version file version pname))
                  (unless (equal file-emacs-version
                                 emacsVersion)
                    (add-error "Minimum Emacs version in the header does not match:
  \"%s\" in file %s
  \"%s\" in package %s" file-emacs-version file emacsVersion pname))
                  (when missing-packages
                    (add-error "Missing dependencies:\n  %s (found in file %s but not in the package)"
                               (string-join missing-packages " ")
                               file)))))
            ;; Check the recipe
            (unless recipe
              (add-error "Recipe is empty"))

            (unless (equal pname (symbol-name (car recipe)))
              (add-error "The package name in the recipe does not match pname:
  \"%s\" in the recipe
  \"%s\" pname" (symbol-name (car recipe)) pname)))
        (error (add-error err))))))

(defun melpa-check-batch-check-package-json ()
  (require 'json)
  (condition-case err
      (dolist (json-file command-line-args-left)
        (unless (file-exists-p json-file)
          (error "File does not exist: %s" json-file))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (packages (with-temp-buffer
                           (insert-file-contents json-file)
                           (goto-char (point-min))
                           (json-read))))
          (mapc #'melpa-check--check-package-config packages)))
    (error (progn
             (message "Error while verifying the package configuration: %s" err)
             (kill-emacs 1))))
  (when melpa-check-package-config-errors
    (message "Errors in the package configuration:\n%s"
             (string-join melpa-check-package-config-errors "\n"))
    (kill-emacs 1)))
