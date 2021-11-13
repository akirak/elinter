;;; elinter.el --- Run elinter -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12") (package-build "0-git"))
;; Keywords: maint lisp
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

;; elinter.el provides a function for copying package recipes
;; from a local copy of MELPA to the local repository for running CI.

;;; Code:

(require 'package-build)
(require 'dash)
(require 'subr-x)
(require 'cl-lib)

(declare-function epkg-provided-by "ext:epkg")
(declare-function epkg-builtin-package-p "ext:epkg")
(declare-function epkg "ext:epkg")
(declare-function find-library-name "find-func")

(defgroup elinter nil
  "Recipe helper for Emacs packages."
  :group 'maint
  :group 'lisp)

(defcustom elinter-recipes-dir
  (bound-and-true-p package-build-recipes-dir)
  "Root directory of your local melpa repository."
  :type 'directory)

(defcustom elinter-copy-recipes t
  "Whether to copy generated recipes to `elinter-recipes-dir'.

If this option is non-nil, `elinter-discover-packages' copies
recipes it creates to `elinter-recipes-dir'.

This is useful for maintaining your own package registry."
  :type 'boolean)

(defcustom elinter-discover-patterns
  '("*.el")
  "List of patterns used to discover source files."
  :type '(repeat string))

(defcustom elinter-recipe-cache-directory
  ".recipes"
  "Directory (relative from the project) to contain recipes."
  :type 'directory)

(defcustom elinter-executable "elinter"
  "Path to the executable file of elinter."
  :type 'file)

(defun elinter--project-root ()
  "Return the project root."
  (locate-dominating-file default-directory ".git"))

(defmacro elinter--require-root (root)
  "Return the ROOT of the project."
  `(or ,root
       (elinter--project-root)
       (cond
        (noninteractive default-directory)
        ((called-interactively-p 'any)
         (read-directory-name "Select the root of the project: "))
        (t
         (user-error "Cannot find root")))))

(defun elinter--expand-file-specs (root specs)
  "Expand file specs in the project.

ROOT is the project, and SPECS is a spec to select files."
  (-map (pcase-lambda (`(,path . _))
          (expand-file-name path root))
        (ignore-errors
          (package-build-expand-file-specs root specs))))

(defun elinter--expand-files-in-recipe (root recipe)
  "Expand file specs in a recipe for a project.

ROOT is the project, and RECIPE is a package recipe."
  (elinter--expand-file-specs root
                              (if-let (file-list (plist-get (cdr recipe) :files))
                                  (if (eq :defaults (car file-list))
                                      (append package-build-default-files-spec
                                              (cdr file-list))
                                    file-list)
                                package-build-default-files-spec)))

(defun elinter--discover-source-files (&optional root)
  "Find elisp source files in ROOT."
  (let ((root (elinter--require-root root)))
    (elinter--expand-file-specs root elinter-discover-patterns)))

(defun elinter--recipes (&optional root)
  "Find existing recipe files in ROOT."
  (let* ((root (elinter--require-root root))
         (dir (expand-file-name elinter-recipe-cache-directory root)))
    (->> (and (file-directory-p dir)
              (directory-files dir t))
         (-map
          (lambda (file)
            (ignore-errors
              (when (file-exists-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (read (current-buffer)))))))
         (delq nil))))

(defun elinter--main-file-p (file)
  "Return non-nil if FILE is a main file."
  (with-temp-buffer
    (insert-file-contents file)
    (lm-header "Package-Requires")))

;;;###autoload
(defun elinter-discover-packages (&optional root)
  "Discover PACKAGES in ROOT."
  (interactive)
  (unless (and (stringp elinter-recipes-dir)
               (file-directory-p elinter-recipes-dir))
    (user-error "First set elinter-recipes-dir to an existing directory containing recipes"))
  (let* ((root (elinter--require-root root))
         (source-files (elinter--discover-source-files root))
         (recipes (elinter--recipes root))
         (covered-files (->> recipes
                             (-map (lambda (recipe)
                                     (elinter--expand-files-in-recipe root recipe)))
                             (apply #'-concat)))
         (uncovered-files (-difference source-files covered-files))
         (new-main-files (-filter #'elinter--main-file-p uncovered-files)))
    (dolist (main-file new-main-files)
      (let* ((package-name (file-name-base main-file))
             (dest-dir (expand-file-name elinter-recipe-cache-directory root))
             (recipe-file (expand-file-name package-name elinter-recipes-dir)))
        (ignore-errors
          (make-directory dest-dir t))
        (unless (or (file-exists-p recipe-file)
                    (find-buffer-visiting recipe-file))
          (with-current-buffer (create-file-buffer recipe-file)
            (let* ((fetcher-spec (elinter--fetcher-spec))
                   ;; TODO: Provide better suggestions for :files spec
                   ;; by reading the dependencies.
                   (files-spec (read-from-minibuffer
                                (format "Enter :files spec for %s (optional): " package-name)
                                ;; If there are multiple packages in the repository,
                                ;; you will probably need :files spec.
                                (when (or recipes
                                          (> (length new-main-files) 1))
                                  (prin1-to-string (list (file-relative-name main-file root))))
                                nil
                                #'read))
                   (recipe (read--expression
                            (format "Confirm recipe for \"%s\": " package-name)
                            (prin1-to-string `(,(intern package-name)
                                               ,@fetcher-spec
                                               ,@(when files-spec
                                                   (list :files files-spec)))))))
              (insert (prin1-to-string recipe))
              (when (search-backward ":files")
                (insert "\n"))
              (setq buffer-file-name recipe-file)
              (setq uncovered-files (cl-set-difference uncovered-files
                                                       (elinter--expand-files-in-recipe
                                                        root
                                                        recipe)
                                                       :test #'string-equal))
              (delay-mode-hooks (emacs-lisp-mode))
              (indent-region (point-min) (point-max))
              (save-buffer))))
        (when elinter-copy-recipes
          (message "Copying %s to %s" package-name dest-dir)
          (copy-file recipe-file (expand-file-name package-name dest-dir)))))))

;;;###autoload
(defmacro elinter-with-packages (root &rest progn)
  "Evaluate something after recipe generation.

This first discover packages in ROOT using `elinter-discover-packages'
and then evaluate PROGN.

If the root is nil, it first looks for one."
  (declare (indent 1))
  `(let* ((root (elinter--require-root ,root))
          (default-directory root))
     (elinter-discover-packages root)
     ,@progn))

;;;###autoload
(defun elinter-discover-packages-and-run (command &optional root)
  "Discover packages in the project and run a command.

COMMAND is a shell command to run, and ROOT is an optional root
of the project."
  (elinter-with-packages root
    (compile command)))

(defvar elinter-args "")

(defvar elinter-args-history nil)

;;;###autoload
(defun elinter (args)
  "Run elinter with ARGS, after discovering packages.

With a universal prefix argument, it prompts for command line
arguments passed to elinter."
  (interactive (list (if current-prefix-arg
                         (read-string "Args: " elinter-args
                                      'elinter-args-history)
                       elinter-args)))
  (elinter-discover-packages-and-run
   (concat (shell-quote-argument elinter-executable) " " args)))

;;;;; Recipe generation helpers (experimental)

;;;;;; Generate :repo/:url spec

(defcustom elinter-user-fetcher 'github
  "Repository hosting service you use for your Emacs Lisp projects."
  :type '(choice (const :tag "github.com" github)
                 (const :tag "gitlab.com" gitlab)
                 (const :tag "Git url" git)))

(defcustom elinter-user-name nil
  "Your login name on the repository service."
  :type '(choice null string))

(defcustom elinter-use-https-url nil
  "Set origin to HTTPS url."
  :type 'boolean)

(defvar-local elinter-fetcher-spec nil
  "Remote repository location set in the buffer.")

(defun elinter--fetcher-spec ()
  "Retrieve or set a remote repository location for the recipe."
  (or elinter-fetcher-spec
      (-some-> (process-lines "git" "config" "--local" "--list")
        (elinter--origin-url-from-config-lines)
        (elinter--url-to-fetcher-spec))
      (let ((spec (elinter--read-fetcher-spec)))
        (if (yes-or-no-p "Set origin of this repository to the spec? ")
            (progn
              (elinter--add-remote "origin"
                                   (elinter--spec-to-url spec))
              spec)
          (setq elinter-fetcher-spec spec)))))

(defsubst elinter--quote-string-for-recipe (string)
  "Quote STRING for use in a package recipe."
  (format "\"%s\"" string))

(defun elinter--build-fetcher-spec (fetcher repo-or-url)
  "Return fetcher and repo components of a package.

This function builds the tail of a recipe, with the package name
and :files spec excluded. Depending on FETCHER, it uses a
different keyword for REPO-OR-URL."
  (cl-ecase fetcher
    ((github gitlab)
     (list :fetcher fetcher :repo repo-or-url))
    (git
     (list :fetcher fetcher :url repo-or-url))))

(defun elinter--default-repo-name (dir)
  "Return a repository name for DIR not including the user name."
  (file-name-nondirectory (string-remove-suffix "/" dir)))

(defun elinter--read-fetcher-spec ()
  "Read a remote repository location as fetcher."
  (elinter--build-fetcher-spec
   elinter-user-fetcher
   (cl-ecase elinter-user-fetcher
     ((github gitlab)
      (read-string (format-message "Repository on %s (in USER/REPO): "
                                   elinter-user-fetcher)
                   (when elinter-user-name
                     (concat elinter-user-name "/"
                             (elinter--default-repo-name default-directory)))))
     (git
      (read-string "Remote Git URL of the repository: ")))))

(defun elinter--break (str needle)
  "Break STR at NEEDLE and return a pair."
  (save-match-data
    (let ((pos (string-match (regexp-quote needle) str)))
      (when pos
        (cons (substring str 0 pos)
              (substring str (+ pos (length needle))))))))

(defun elinter--origin-url-from-config-lines (lines)
  "Find the URL of origin from LINES of git config."
  (-some--> (-find (lambda (s) (string-prefix-p "remote.origin.url=" s))
                   lines)
    (elinter--break it "=")
    (cdr it)))

(defun elinter--url-to-fetcher-spec (git-url)
  "Build a repository location spec for the recipe from GIT-URL."
  (save-match-data
    (cond
     ((string-match (rx bol (or "git@github.com:" "https://github.com/")
                        (group (+? anything))
                        (? ".git")
                        (? "/")
                        eol)
                    git-url)
      (elinter--build-fetcher-spec 'github (match-string 1 git-url)))
     ((string-match (rx bol (or "git@gitlab.com:" "https://gitlab.com/")
                        (group (+? anything))
                        (? ".git")
                        (? "/")
                        eol)
                    git-url)
      (elinter--build-fetcher-spec 'gitlab (match-string 1 git-url)))
     ;; TODO: Add support for BitBucket and other forges
     (t
      (elinter--build-fetcher-spec 'git git-url)))))

(defun elinter--add-remote (name git-url)
  "Add a remote named NAME to GIT-URL to the repository."
  (let ((result (call-process "git" nil nil nil "remote" "add" name git-url)))
    (unless (= result 0)
      (error "Non-zero exit code while setting remote %s to %s" name git-url))))

(defun elinter--spec-to-url (spec)
  "Convert a repository SPEC to URL."
  (cl-ecase (plist-get spec :fetcher)
    (git (plist-get spec :url))
    ((github gitlab) (format (if elinter-use-https-url
                                 "https://%s/%s.git"
                               "git@%s:%s.git")
                             (cl-ecase (plist-get spec :fetcher)
                               (github "github.com")
                               (gitlab "gitlab.com"))
                             (plist-get spec :repo)))))

;;;; Generate a Package-Requires header for the current main file

(defun elinter--package-requires ()
  "Generate a list of required packages for the current main file."
  (let* ((package-name (file-name-base (buffer-file-name)))
         (recipe-file (expand-file-name package-name ".recipes"))
         (recipe (with-temp-buffer
                   (insert-file-contents recipe-file)
                   (goto-char (point-min))
                   (read (current-buffer))))
         (files (elinter--expand-files-in-recipe default-directory recipe))
         features
         packages)
    (dolist (file files)
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward (rx bol "(require '") nil t)
              (push (intern (thing-at-point 'symbol t)) features))))))
    (dolist (feature (cl-remove-duplicates features))
      (push (epkg-provided-by feature) packages))
    (->> (cl-remove-duplicates packages :test #'string-equal)
         (-non-nil)
         (-filter (lambda (package-name)
                    (not (epkg-builtin-package-p (epkg package-name))))))))

(defun elinter--package-requires-with-versions ()
  "Generate a list of required packages with versions."
  (-map (lambda (library)
          (let ((path (find-library-name library)))
            (with-current-buffer (or (find-buffer-visiting path)
                                     (find-file-noselect path))
              (save-excursion
                (goto-char (point-min))
                (let ((version (or (lm-header "Version")
                                   (lm-header "Package-Version"))))
                  (list (intern library)
                        (if version
                            (--> (version-to-list version)
                                 (-take 2 it)
                                 (-take-while (lambda (n) (>= n 0)) it)
                                 (-map #'number-to-string it)
                                 (string-join it "."))
                          "0")))))))
        (elinter--package-requires)))

;;;###autoload
(defun elinter-generate-package-requires ()
  "Insert \"Package-Requires\" header for the current file."
  (interactive)
  (require 'epkg)
  (goto-char (point-min))
  (when (or (re-search-forward (lm-get-header-re "Package-Requires") nil t)
            (re-search-forward (lm-get-header-re "Version") nil t))
    (beginning-of-line 1))
  (let* ((emacs (or (assoc 'emacs (read (lm-header-multiline "Package-Requires")))
                    ;; Use the running Emacs version as fallback.
                    (list 'emacs emacs-version)))
         (package-deps (elinter--package-requires-with-versions))
         (package-requires (cons emacs package-deps)))
    (insert ";; Package-Requires: " (prin1-to-string package-requires) "\n")))

(provide 'elinter)
;;; elinter.el ends here
