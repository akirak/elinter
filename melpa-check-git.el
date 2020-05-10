;;; melpa-check-git.el --- Git support for melpa-check -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Version: 0.1
;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Package-Requires: ((emacs "26.1") (s "1.12") (dash "2.17"))
;; Keywords: maint vc
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

;; This library provides some utilities for melpa-check.el related to
;; Git.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(defgroup melpa-check-git nil
  "Git-related facilities for melpa-check."
  :group 'melpa-check
  :group 'git)

(defcustom melpa-check-git-user-fetcher 'github
  "Repository hosting service you use for your Emacs Lisp projects."
  :type '(choice (const :tag "github.com" github)
                 (const :tag "gitlab.com" gitlab)))

(defcustom melpa-check-git-user-name nil
  "Your login name on the repository service."
  :type '(choice null string))

(defcustom melpa-check-git-use-https-url nil
  "Set origin to HTTPS url."
  :type 'boolean)

(defvar-local melpa-check-git-fetcher-spec nil
  "Remote repository location set in the buffer.")

(defun melpa-check-git--fetcher-spec ()
  "Retrieve or set a remote repository location for the recipe."
  (or melpa-check-git-fetcher-spec
      (-some-> (melpa-check-git--origin-url)
        (melpa-check-git--url-to-fetcher-spec))
      (let ((spec (melpa-check-git--read-fetcher-spec)))
        (if (yes-or-no-p "Set origin of this repository to the spec? ")
            (progn
              (melpa-check-git--add-remote "origin"
                                           (melpa-check-git--spec-to-url spec))
              spec)
          (setq melpa-check-git-fetcher-spec spec)))))

(defun melpa-check-git--read-fetcher-spec ()
  "Read a remote repository location as fetcher."
  (cond
   (melpa-check-git-user-fetcher
    (let ((fetcher melpa-check-git-user-fetcher)
          (user melpa-check-git-user-name))
      (list :fetcher fetcher
            :repo (read-string (format-message "Repository on %s: " fetcher)
                               (when user
                                 (concat user "/"))))))
   (t
    (error "The default fetcher is nil"))))

(defun melpa-check-git--origin-url ()
  "Return the URL of the origin of this repository."
  (-some--> (-find (lambda (s) (string-prefix-p "remote.origin.url=" s))
                   (process-lines "git" "config" "--local" "--list"))
    (s-split-up-to "=" it 1)
    (nth 1 it)))

(defun melpa-check-git--url-to-fetcher-spec (git-url)
  "Build a repository location spec for the recipe from GIT-URL."
  (save-match-data
    (cond
     ((string-match (rx bol (or "git@github.com:" "https://github.com/")
                        (group (+? anything)) ".git" eol)
                    git-url)
      `(:fetcher github :repo ,(match-string 1 git-url)))
     ((string-match (rx bol (or "git@gitlab.com:" "https://gitlab.com/")
                        (group (+? anything)) ".git" eol)
                    git-url)
      `(:fetcher gitlab :repo ,(match-string 1 git-url)))
     ;; TODO: Add support for BitBucket and other forges
     (t
      `(:fetcher git :url ,git-url)))))

(defun melpa-check-git--add-remote (name git-url)
  "Add a remote named NAME to GIT-URL to the repository."
  (let ((result (call-process "git" nil nil nil "remote" "add" name git-url)))
    (unless (= result 0)
      (error "Non-zero exit code while setting remote %s to %s" name git-url))))

(defun melpa-check-git--spec-to-url (spec)
  "Convert a repository SPEC to URL."
  (cl-ecase (plist-get spec :fetcher)
    (git (plist-get spec :url))
    ((github gitlab) (format (if melpa-check-git-use-https-url
                                 "https://%s/%s.git"
                               "git@%s:%s.git")
                             (cl-ecase (plist-get spec :fetcher)
                               (github "github.com")
                               (gitlab "gitlab.com"))
                             (plist-get spec :repo)))))

(provide 'melpa-check-git)
;;; melpa-check-git.el ends here
