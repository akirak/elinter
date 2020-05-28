;;; melpa-check.el --- Configure packages for CI -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (f "0.20") (dash "2.17") (s "1.12"))
;; Keywords: maint files

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

;; melpa-check is a Nix-based CI framework for Emacs Lisp packages.
;;
;; melpa-check.el is an Emacs frontend to the framework and mainly
;; provides functionalities for configuration.

;;; Code:

(unless (memq system-type '(gnu/linux darwin))
  (error "This system does not support Nix, which is required by this package"))

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'f)
(require 'project)
(require 'json)

(require 'melpa-check-multisel)
(require 'melpa-check-package)

(declare-function dhall-format-buffer "ext:dhall-mode")

(defgroup melpa-check nil
  "Configure and run melpa-check, a CI framework for Emacs Lisp."
  :group 'files
  :group 'maint
  :prefix "melpa-check")

;;;; Custom variables

(defcustom melpa-check-nix-executable "nix"
  "Executable file for nix."
  :type 'file)

(defcustom melpa-check-nix-instantiate-executable "nix-instantiate"
  "Executable file for \"nix-instantiate\"."
  :type 'file)

(defcustom melpa-check-niv-executable "niv"
  "Executable file for niv."
  :type 'file)

(defcustom melpa-check-yq-executable "yq"
  "Executable file for yq."
  :type 'file)

(defcustom melpa-check-dhall-to-json-executable "dhall-to-json"
  "Executable file for dhall-to-json."
  :type 'file)

(defcustom melpa-check-schema-url
  "https://raw.githubusercontent.com/akirak/melpa-check/v3/schema.dhall"
  "Source URL of schema.dhall."
  :type 'string)

(defcustom melpa-check-skip-test-files t
  "Whether to exclude test files when you select files of a package."
  :type 'boolean)

(defcustom melpa-check-github-actions-config-template
  "let Actions = https://raw.githubusercontent.com/akirak/melpa-check/v3/dhall/github-actions.dhall

let packages = ../packages.dhall

-- Alternatively, you can use 'Actions.MultiFileCiConfig.default'
let config = Actions.MultiFileCiConfig::{
-- Add your preferences here
}

in  Actions.buildMultiFileCiWorkflows config packages"
  "Default content of Dhall CI configuration for GitHub Actions."
  :type 'string)

(defface melpa-check-success-face
  '((t :foreground "green2"))
  "The face used for success messages."
  :group 'melpa-check)

;;;; Variables

(defconst melpa-check-log-buffer "*melpa-check log*")

(defvar-local melpa-check-dont-use-niv nil
  "Don't add/use melpa-check to the niv sources in this project.")

;;;; Machinery

;;;;; Logging

(defun melpa-check--log-buffer ()
  "Return a read-only buffer named `melpa-check-log-buffer'."
  (or (get-buffer melpa-check-log-buffer)
      (let ((newbuf (generate-new-buffer melpa-check-log-buffer)))
        (with-current-buffer newbuf
          (read-only-mode 1))
        newbuf)))

(defun melpa-check--log (msg &rest args)
  "Log MSG with ARGS to `melpa-check-log-buffer'."
  (with-current-buffer (melpa-check--log-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (apply #'format-message msg args) "\n"))))

(defmacro melpa-check--with-log (&rest progn)
  "Evaluate PROGN with output to `melpa-check-log-buffer' enabled."
  `(let ((logbuf (melpa-check--log-buffer)))
     (setf (buffer-local-value 'inhibit-read-only logbuf) t)
     (unwind-protect
         (progn
           ,@progn)
       (setf (buffer-local-value 'inhibit-read-only logbuf) nil)
       (with-current-buffer logbuf
         (goto-char (point-max))))))

(defun melpa-check--clear-log ()
  "Erase the content of the log buffer."
  (with-current-buffer (melpa-check--log-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;;;; Wrapping IO functions with logging enabled
(defmacro melpa-check--def-logged (func msgfunc)
  "Define a wrapper for FUNC which log information formatted by MSGFUNC."
  (declare (indent 1))
  `(defun ,(intern (concat "melpa-check--" (symbol-name func))) (&rest args)
     ,(format "Logged version of `%s' (with ARGS)." func)
     (melpa-check--log (apply ,msgfunc args))
     (apply (quote ,func) args)))

(melpa-check--def-logged make-directory
  (cl-function
   (lambda (dir &optional _parents)
     (format-message "Create directory %s" dir))))

(melpa-check--def-logged make-symbolic-link
  (cl-function
   (lambda (target linkname &optional _ok-if-already-exists)
     (format-message "Link: %s -> %s" linkname target))))

(melpa-check--def-logged url-copy-file
  (cl-function
   (lambda (url newname &rest _args)
     (format-message "Downloading %s to %s" url
                     (f-short newname)))))

(melpa-check--def-logged copy-file
  (cl-function
   (lambda (file newname &rest _args)
     (format-message "Copy %s to %s"
                     (f-short file)
                     (f-short newname)))))

(melpa-check--def-logged delete-file
  (cl-function
   (lambda (file &rest _args)
     (format-message "Delete %s" (f-short (f-expand file))))))

(melpa-check--def-logged save-buffer
  (cl-function
   (lambda (&rest _args)
     (format-message "Save buffer to %s (length %d)"
                     (f-short (f-relative (buffer-file-name)))
                     (buffer-size)))))

;;;; Initializing a project

;;;###autoload
(defun melpa-check-init-project ()
  "Initialize configuration files for an Emacs Lisp project."
  (interactive)
  (let* ((root (or (melpa-check--project-root)
                   (let ((default-directory (read-directory-name "Project root: " nil nil t)))
                     (when (yes-or-no-p (format "Initialize a Git repository at %s? "
                                                default-directory))
                       (melpa-check--call-process "git" "init"))
                     default-directory)))
         (multi (cl-ecase (read-char-choice "Is this a multi-package repository? [y/n, default: n]: "
                                            (cons 13 (string-to-list "yn")))
                  (?y t)
                  ((?n 13) nil)))
         (config-dir (if (yes-or-no-p (format-message
                                       "Use the default .melpa-check configuration directory (in %s)? "
                                       (f-short root)))
                         ".melpa-check"
                       (read-directory-name "Configuration directory: "
                                            root))))
    (melpa-check--init-project :root (expand-file-name root)
                               :multi multi
                               :config-dir (expand-file-name config-dir root))))

(cl-defun melpa-check--init-project (&key root multi config-dir)
  "Internal function for initializing a project.

ROOT, MULTI, and CONFIG-DIR should be passed from
`melpa-check-init-project'."
  (cl-check-type root file-directory)
  (cl-check-type root file-name-absolute)
  (cl-check-type multi boolean)
  (cl-check-type config-dir string)
  (cl-check-type config-dir file-name-absolute)
  (melpa-check--clear-log)
  (melpa-check--log "Initializing a project %s with config at %s (multi: %s, niv: %s)"
                    (f-short root)
                    (f-relative config-dir root)
                    (if multi "yes" "no")
                    (if melpa-check-dont-use-niv "no" "yes"))
  (let ((relative (file-relative-name config-dir root))
        (default-directory (expand-file-name root)))
    ;; Add melpa-check to Nix sources
    (unless melpa-check-dont-use-niv
      (message "Initializing nix sources...")
      (unless (f-exists-p (f-join "nix" "sources.nix"))
        (melpa-check--log "nix/sources.nix is not found. Initializing niv...")
        (melpa-check--niv-sync "init"))
      ;; Check if melpa-check is already added before adding it
      (unless (eq t (melpa-check--nix-eval "(import nix/sources.nix) ? melpa-check"))
        (melpa-check--niv-sync "add" "akirak/melpa-check" "--branch" "v3"))
      ;; Add the latest unstable version of nixpkgs
      (unless (eq t (melpa-check--nix-eval "(import nix/sources.nix) ? nixpkgs-unstable"))
        (melpa-check--niv-sync "add" "NixOS/nixpkgs-channels" "-n" "nixpkgs-unstable"
                               "--branch" "nixpkgs-unstable")))
    ;; Create the configuration directory
    (cond
     ((not (file-exists-p config-dir))
      (melpa-check--make-directory config-dir))
     ((not (yes-or-no-p (format-message "%s already exists. Override? "
                                        (f-short config-dir))))
      (user-error "Aborting")))
    ;; Create a symbolic to the configuration directory
    (cond
     ((string-match-p (rx bol ".melpa-check" (optional "?") eol)
                      relative)
      (when (file-symlink-p ".melpa-check-tmp")
        (melpa-check--delete-file ".melpa-check-tmp")))
     (t
      (melpa-check--make-symbolic-link relative ".melpa-check-tmp" t)))
    ;; Copy the schema file
    (let ((schema-out (f-join relative "schema.dhall")))
      (if melpa-check-dont-use-niv
          (melpa-check--url-copy-file melpa-check-schema-url schema-out t)
        (let* ((melpa-check-root (melpa-check--nix-build-expr
                                  "toString (import ./nix/sources.nix).melpa-check"))
               (src (f-join melpa-check-root "schema.dhall")))
          ;; Copy schema.dhall, unless
          ;; (1) there is no existing file in the output
          ;; (2) or the content is different and the user permits
          (when (or (not (f-exists-p schema-out))
                    (and (not (equal (melpa-check--file-sha1 src)
                                     (melpa-check--file-sha1 schema-out)))
                         (yes-or-no-p "Overwrite the existing schema.dhall?")
                         (progn
                           (melpa-check--delete-file schema-out)
                           t)))
            (melpa-check--copy-file src schema-out t)))))
    ;; Create a package configuration
    (with-temp-buffer
      (setq buffer-file-name (f-join config-dir "default.nix"))
      (insert (melpa-check--build-default-nix relative))
      (melpa-check--save-buffer))
    ;; If the user aborts operation, the temporary buffer is killed.
    (with-temp-buffer
      (setq buffer-file-name (f-join config-dir "packages.dhall"))
      (insert (if multi
                  (melpa-check--build-multi-package-config)
                (melpa-check--build-single-package-config)))
      (melpa-check--save-buffer))
    (find-file (f-join config-dir "packages.dhall"))
    (message (propertize "Project configured!"
                         'font-lock-face
                         'melpa-check-success-face))))

(cl-defun melpa-check--build-default-nix (relative-config-dir)
  "Generate a content for default.nix in RELATIVE-CONFIG-DIR."
  (cl-check-type relative-config-dir f-relative)
  (when (string-prefix-p ".." relative-config-dir)
    (error "Configuration directory must be under the project root: %s"
           relative-config-dir))
  (let* ((relative-config-dir (f-slash relative-config-dir))
         (packageFile (f-join relative-config-dir "packages.dhall"))
         (srcDir (concat (f-slash (f-relative "" relative-config-dir)) "."))
         (local-source (concat "import (import "
                               (f-relative "nix/sources.nix" relative-config-dir)
                               ").%s"))
         (melpa-check (if melpa-check-dont-use-niv
                          (upcase "fixme")
                        (format local-source "melpa-check")))
         (nixpkgs-unstable (concat (format local-source "nixpkgs-unstable")
                                   " { }")))
    ;; Generate a string for a function that takes optional arguments
    ;; and call melpa-check.
    (concat "{\n"
            ;; Generate a list of function arguments
            (mapconcat (lambda (l)
                         (concat "  # " (nth 0 l) "\n"
                                 "  " (nth 1 l) " ? " (nth 2 l)))
                       `(("The default version of Emacs to use"
                          "emacs" "\"snapshot\"")
                         ("You can use niv to update melpa-check"
                          "melpa-check" ,(or melpa-check "null"))
                         ;; TODO: Add support for custom emacs-ci
                         ("The directory containing source files"
                          "srcDir" ,srcDir)
                         ("Custom nixpkgs for Emacs packages"
                          "pkgs" ,nixpkgs-unstable)
                         ("A configuration file which defines packages under test"
                          "packageFile" ,(concat "\"" packageFile "\"")))
                       ",\n")
            "\n}:\n"
            "melpa-check {\n  inherit emacs packageFile srcDir;\n"
            "  emacsPackages = pkgs.emacsPackages;\n"
            "}")))

(defun melpa-check--build-multi-package-config ()
  "Build a content of packages.dhall for multiple packages."
  (let* ((all-files (melpa-check--elisp-files))
         (remaining-files (copy-sequence all-files))
         files1 packages)
    ;; Select source files for each package and build a data for it.
    ;;
    ;; Finish when no files remain unselected or no file is selected.
    (while (and remaining-files
                (setq files1 (melpa-check-multisel "Source files for a package (nil to finish): "
                                                   remaining-files)))
      ;; Build a package data and append to the list of packages.
      ;;
      ;; At this point, localDependencies is unfilled.
      (push (melpa-check-package--from-files files1
                                             :multi-package t
                                             :all-files all-files)
            packages)
      (setq remaining-files (cl-set-difference remaining-files files1 :test #'string-equal)))
    ;; Fill in localDependencies field
    (melpa-check--fill-local-dependencies packages)
    ;; Serialize
    (melpa-check--serialize-package-config packages)))

(defun melpa-check--build-single-package-config ()
  "Build a content of packages.dhall for a single package."
  (let* ((files (melpa-check--elisp-files))
         (package (melpa-check-package--from-files
                   (melpa-check-multisel "Source files for the package: "
                                         files))))
    (melpa-check--serialize-package-config (list package))))

(defun melpa-check--fill-local-dependencies (packages)
  "Fill local dependencies in PACKAGES."
  (let ((local-package-names (-map #'melpa-check-package-pname packages)))
    (mapc (lambda (package)
            (setf (melpa-check-package-localDependencies package)
                  (-intersection
                   local-package-names
                   (melpa-check-package-dependencies package))))
          packages)))

(defun melpa-check--serialize-package-config (packages)
  "Serialize a list of PACKAGES into Dhall format."
  (cl-check-type packages list)
  (with-temp-buffer
    (cl-labels
        ((format-package
          (package)
          (cl-check-type package melpa-check-package)
          (concat "Package::{\n"
                  (cl-loop for (key . value) in (melpa-check-package--to-alist package)
                           when (or value
                                    ;; Allow empty value
                                    (memq key '(dependencies
                                                testDrivers)))
                           concat (format "  , %s = %s\n"
                                          key (format-value key value)))
                  "}\n"))
         (format-value
          (key value)
          (cl-case key
            (recipe (concat "''\n" value "\n''"))
            (mainFile (concat "Some " (serialize-obj value)))
            (testDrivers (if value
                             (format "[ %s ]"
                                     (--map (s-prepend "TestDriver." it) value))
                           "[] : List TestDriver"))
            ((dependencies buttercupTests) (if value
                                               (serialize-obj value)
                                             "[] : List Text"))
            (otherwise (serialize-obj value))))
         (serialize-obj
          (obj)
          (cl-typecase obj
            (null "None")
            (list (format "[ %s ]" (mapconcat #'serialize-obj
                                              obj ", ")))
            (otherwise (prin1-to-string obj)))))
      (insert "let Schema = ./schema.dhall\n\n"
              "let Package = Schema.Package\n\n"
              "let TestDriver = Schema.TestDriver\n\n"
              "in ["
              (mapconcat #'format-package packages ", ")
              "]")
      (buffer-string)
      (let ((src (buffer-string)))
        ;; Format the buffer if dhall is available
        (condition-case-unless-debug err
            (progn
              (require 'dhall-mode)
              (dhall-format-buffer)
              (melpa-check--log "Successfully formatted using dhall-format")
              (buffer-string))
          ;; Return the original source if it fails
          (error (progn
                   (melpa-check--log "dhall failed but continue anyway: %s" err)
                   src)))))))

;;;; Running tasks
;;;###autoload
(defun melpa-check-config (&optional dir)
  "Check the validity of the configuration using \"melpa-check config\".

With a universal prefix, reset the configuration directory to DIR."
  (interactive (list (when current-prefix-arg
                       (read-directory-name "Configuration directory: "
                                            nil nil t))))
  (melpa-check--compile-project "melpa-check"
    "config"
    (when dir
      `("-f" ,(f-relative dir (melpa-check--project-root))))))

;;;###autoload
(defun melpa-check-run-all ()
  "Run \"melpa-check all\" command in the project."
  (interactive)
  (melpa-check--compile-project "melpa-check"
    "all"))

(defun melpa-check--compile-project (cmd &rest args)
  "Run CMD with ARGS using `compile' at the project root."
  (declare (indent 1))
  (let ((default-directory (or (melpa-check--project-root)
                               (error "Missing project root"))))
    (apply #'melpa-check--compile-process
           cmd (remq nil (-flatten args)))))

(defun melpa-check--compile-process (cmd &rest args)
  "Compile with CMD and ARGS."
  (melpa-check--compile (concat cmd " "
                                (mapconcat #'shell-quote-argument
                                           args " "))))

(defun melpa-check--compile (command)
  "Run COMMAND using `compile'."
  (melpa-check--log "Running command in compile buffer [dir %s]: %s"
                    (f-short default-directory)
                    command)
  (compile command))

;;;; Initialize CI configuration
;;;###autoload
(defun melpa-check-init-github-actions ()
  "Initialize CI configuration for GitHub Actions."
  (interactive)
  (let* ((root (or (melpa-check--project-root)
                   (user-error "Initialize a Git repository first")))
         (config-dir (melpa-check--config-dir root))
         (ci-config-dir (f-join config-dir "ci"))
         (ci-config-file (f-join ci-config-dir "github.dhall")))
    (unless (f-directory-p ci-config-dir)
      (make-directory ci-config-dir))
    (when (f-exists-p ci-config-file)
      (find-file ci-config-file)
      (user-error "File already exists: %s" ci-config-file))
    (with-current-buffer (find-file-noselect ci-config-file)
      (insert melpa-check-github-actions-config-template)
      (save-buffer)
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun melpa-check-generate-ci-config ()
  "Generate configuration files for CI."
  (interactive)
  (let* ((root (or (melpa-check--project-root)
                   (user-error "Initialize a Git repository first")))
         (config-dir (melpa-check--config-dir root))
         (ci-config-dir (f-join config-dir "ci"))
         (files (directory-files ci-config-dir t (rx ".dhall" eol))))
    (dolist (ci-config-file files)
      (let* ((src (melpa-check--dhall-to-json ci-config-file))
             (json-object-type 'alist)
             (json-array-type 'list)
             (data (with-temp-buffer
                     (insert src)
                     (goto-char (point-min))
                     (json-read)))
             (filenames (->> (alist-get 'files data)
                             (--map (alist-get 'fileName it)))))
        (dolist (dir (alist-get 'directories data))
          (unless (f-directory-p (f-join root dir))
            (make-directory (f-join root dir) t)))
        ;; To convert the configuration to YAML, you need to access
        ;; objects with number indices
        (dolist (i (number-sequence 0 (1- (length filenames))))
          (when-let
              (skipped
               (catch 'skip
                 (let* ((outfile (f-join root (nth i filenames)))
                        (buf (find-buffer-visiting outfile)))
                   ;; If there is an open buffer and it's modified,
                   ;; ask if the user wants to discard the changes
                   (when (buffer-modified-p buf)
                     (if (yes-or-no-p (format-message
                                       "Already visiting %s and it's modified. Discard the changes?"
                                       (f-short outfile)))
                         (progn
                           (with-current-buffer buf
                             (set-buffer-modified-p nil))
                           (kill-buffer buf))
                       (throw 'skip outfile)))
                   ;; If the buffer has been killed, set the variable to nil
                   (when (and buf (not (buffer-live-p buf)))
                     (setq buf nil))
                   (with-current-buffer (or buf
                                            (find-file-noselect outfile))
                     ;; If the buffer has an existing content,
                     ;; ask if the user wants to replace it
                     (widen)
                     (when (> (buffer-size) 0)
                       (if (yes-or-no-p (format-message
                                         "Replace content in %s? " (f-short outfile)))
                           (erase-buffer)
                         (throw 'skip outfile)))
                     ;; Insert the JSON representation of the Dhall configuration
                     ;; and convert it to YAML using yq
                     (insert src)
                     (melpa-check--yq-on-buffer "-M" "-y" (format ".files | .[%d] | .content" i))
                     (set-auto-mode)
                     (save-buffer)
                     ;; Display the buffer so the user can edit it
                     (switch-to-buffer (current-buffer))
                     nil))))
            (message "Skipped generating CI configuration to %s"
                     (f-short skipped))))))))

;;;; Utility functions

(defun melpa-check--file-sha1 (file)
  "Retrieve sha1sum of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (sha1 (current-buffer))))

;;;;; Project files

(defun melpa-check--project-root ()
  "Return the version control root of the current file, if any."
  (pcase (project-current)
    (`(vc . ,root) root)))

(defun melpa-check--config-dir (root)
  "Get the configuration directory in an existing project ROOT."
  (cond
   ((f-symlink-p (f-join root ".melpa-check-tmp"))
    (file-truename ".melpa-check-tmp"))
   ((f-directory-p (f-join root ".melpa-check"))
    (f-join root ".melpa-check"))
   (t
    (user-error "First initialize a project using melpa-check-init-project"))))

(defun melpa-check--elisp-files ()
  "Recursively retrieve elisp sources files in the directory."
  (let* ((root (expand-file-name (file-name-as-directory default-directory)))
         (files (->> (directory-files-recursively root
                                                  (rx bol (not (any ".")) (+ anything)
                                                      ".el" eol))
                     (--map (string-remove-prefix root it))
                     (--filter (not (string-match-p (rx (or "/." (and bol "."))) it)))
                     (-sort (lambda (a b)
                              (string< (file-name-sans-extension a)
                                       (file-name-sans-extension b)))))))
    (if melpa-check-skip-test-files
        (cl-remove-if #'melpa-check--like-test-file-p files)
      files)))

(defun melpa-check--like-test-file-p (file)
  "Return non-nil if FILE look like an elisp test file."
  (or (string-match-p (rx bol "test" (optional "s") "/") file)
      (string-match-p (rx "-test" (optional "s") ".el" eol) file)))

;;;; External processes

(defun melpa-check--call-process (cmd &rest args)
  "Run CMD with ARGS and log its output to `melpa-check-log-buffer'."
  (let ((command (concat cmd " " (mapconcat #'shell-quote-argument args " "))))
    (melpa-check--log "Running \"%s\"" command)
    (let ((result (melpa-check--with-log
                   (apply #'call-process cmd nil melpa-check-log-buffer nil
                          args))))
      (melpa-check--log "%s" result)
      (unless (= 0 result)
        (error "Non-zero exit from \"%s\"" command)))))

(defun melpa-check--read-process (cmd &rest args)
  "Run CMD with ARGS and log its output to `melpa-check-log-buffer'."
  (let ((command (concat cmd " " (mapconcat #'shell-quote-argument args " "))))
    (melpa-check--log "Reading output from \"%s\"" command)
    (with-temp-buffer
      (let ((status (apply #'call-process cmd
                           ;; Discard stderr.
                           nil (list (current-buffer) nil)
                           nil
                           args)))
        (if (eq status 0)
            (buffer-substring-no-properties (point-min) (point-max))
          (error "Non-zero exit from \"%s\"" command))))))

(defun melpa-check--json-read-string (str)
  "Parse STR as JSON."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (json-read-from-string str)))

(defun melpa-check--build-maybe-nix-command (executable
                                             default-executable
                                             nix-shell-args
                                             &rest args)
  "Build a command possibly run inside \"nix-shell\".

This returns a list of a command and arguments which can be
wrapped with \"nix-shell\" if EXECUTABLE is unavailable.

In that case, DEFAULT-EXECUTABLE is used as the executable name,
and NIX-SHELL-ARGS which is a list of strings are passed to
\"nix-shell\" program.

In any case, ARGS will be passed to the program."
  (if (or (f-executable-p executable)
          (executable-find executable))
      (cons executable args)
    `("nix-shell"
      ,@nix-shell-args
      "--command"
      ,(mapconcat #'shell-quote-argument
                  (cons default-executable args)
                  " "))))

(defun melpa-check--yq-on-buffer (&rest args)
  "Replace the buffer content with the output of yq, with ARGS."
  (-let (((cmd . args2) (apply #'melpa-check--build-maybe-nix-command
                               melpa-check-yq-executable "yq"
                               '("-p" "yq")
                               args)))
    (apply #'call-process-region (point-min) (point-max)
           cmd
           'delete (list (current-buffer) nil) nil
           args2)))

(defun melpa-check--dhall-to-json (file)
  "Run \"dhall-to-json\" program on FILE."
  (-let (((cmd . args) (melpa-check--build-maybe-nix-command
                        melpa-check-dhall-to-json-executable
                        "dhall-to-json"
                        '("-A" "dhall-json-simple" "https://github.com/justinwoo/easy-dhall-nix/archive/master.tar.gz")
                        "--file" file)))
    (apply #'melpa-check--read-process cmd args)))

;;;;; Nix

(defun melpa-check--nix-installed-p ()
  "Check if Nix is installed on the system.

If the system type is unsupported by Nix, it throws an error."
  (cl-case system-type
    ((gnu/linux darwin)
     (executable-find melpa-check-nix-executable))
    (otherwise
     (user-error "This package does not work on your system, since it depends on Nix, which works only on gnu/linux and darwin now.  Your system: %s" system-type))))

(defun melpa-check--nix-build-expr (expr)
  "Instantiate EXPR and realise its result."
  (let ((path (melpa-check--nix-eval expr)))
    (string-trim-right
     (melpa-check--read-process "nix-store"
                                "-r"
                                path))))

(defun melpa-check--nix-eval (expr)
  "Evaluate EXPR using \"nix-instantiate\" command."
  (melpa-check--log "Evaluating Nix expression %s" expr)
  (melpa-check--json-read-string
   (melpa-check--read-process melpa-check-nix-instantiate-executable
                              "--eval"
                              "--json"
                              "--expr" expr)))

;;;;; Niv

(defun melpa-check--niv-sources (&optional root)
  "Read nix/sources.nix maintained by niv.

You can specify the repository root as ROOT."
  (let ((file (f-join (or root (melpa-check--project-root))
                      "nix" "sources.json")))
    (when (f-exists-p file)
      (let ((json-object-type 'alist)
            (json-array-type 'list))
        (json-read-file file)))))

(defun melpa-check--has-niv-source-p (&optional root)
  "Check if melpa-check is in the niv sources.

You can specify the repository root as ROOT."
  (alist-get 'melpa-check (melpa-check--niv-sources root)))

(defun melpa-check--niv-sync (&rest args)
  "Call niv synchronously with ARGS."
  (apply #'melpa-check--call-process
         melpa-check-niv-executable
         args))

(provide 'melpa-check)
;;; melpa-check.el ends here
