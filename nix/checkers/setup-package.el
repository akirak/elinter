;;; setup-package.el --- Set up package.el for testing -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
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

;; This configures package.el for running package-lint and testing
;; inside melpa-check.
;;
;; The logic is based on an implementation in makel:
;; <https://gitlab.petton.fr/DamienCassou/makel/blob/master/makel.mk>

;;; Code:

(require 'subr-x)

;;;; Configure package.el
(setq load-prefer-newer t)
(require 'package)
;; Configure the standard set of package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))

;; Store packages in a separate directory to avoid polution of
;; user-emacs-directory.
(cond
 ((eq system-type 'gnu/linux)
  (setq package-user-dir
        ;; If you want to store packages in a specific directory,
        ;; e.g. for caching on CI, you can give it as an environment
        ;; variable.
        (or (getenv "ELPA_USER_DIR")
            (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                                 (expand-file-name "~/.cache"))))
              (expand-file-name (concat "melpa-check/elpa/"
                                        (format-time-string "%F")
                                        "/" emacs-version)
                                xdg-cache)))))
 ((eq system-type 'darwin)
  (setq package-user-dir
        ;; If you want to store packages in a specific directory,
        ;; e.g. for caching on CI, you can give it as an environment
        ;; variable.
        (or (getenv "ELPA_USER_DIR")
            (expand-file-name (concat "~/Library/Caches/melpa-check/elpa/"
                                      (format-time-string "%F")
                                      "/" emacs-version)))))
 (t
  (when (getenv "ELPA_USER_DIR")
    (setq package-user-dir (getenv "ELPA_USER_DIR")))))

;; Download the package database only once
(if (file-directory-p package-user-dir)
    (package-initialize)
  (make-directory package-user-dir t)
  (package-initialize)
  (package-refresh-contents))
;; Disable the local variables for packages
(setq enable-dir-local-variables nil)

(defun setup-package-many (packages)
  "Install PACKAGES that are missing in the current environment."
  (mapc (lambda (sym)
          (unless (package-installed-p sym)
            (package-install sym)))
        packages))

(provide 'setup-package)
;;; setup-package.el ends here
