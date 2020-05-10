;;; package-lint-runner.el --- Run package-lint in Nix -*- lexical-binding: t -*-

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

;; This script runs package-lint inside Nix.

;;; Code:

;; The logic is based on an implementation in makel:
;; <https://gitlab.petton.fr/DamienCassou/makel/blob/master/makel.mk>

(require 'subr-x)

;;;; Configure package.el
(setq load-prefer-newer t)
(require 'package)
;; Configure the standard set of package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Store packages in a separate directory to avoid polution of
;; user-emacs-directory.
(when (eq system-type 'gnu/linux)
  (setq package-user-dir
        ;; If you want to store packages in a specific directory,
        ;; e.g. for caching on CI, you can give it as an environment
        ;; variable.
        (or (getenv "ELPA_USER_DIR")
            (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                                 (expand-file-name "~/.cache"))))
              (expand-file-name "melpa-check/elpa" xdg-cache)))))

;; Download the package database only once
(if (file-directory-p package-user-dir)
    (package-initialize)
  (make-directory package-user-dir t)
  (package-initialize)
  (package-refresh-contents))
;; Disable the local variables for packages
(setq enable-dir-local-variables nil)

;;;; Run package-lint
;; package-lint is not installed by package.el.
;; You need to use an Emacs package with the linter.
;; This is provided by nix.
(require 'package-lint)
(message "package-user-dir: %s" (abbreviate-file-name package-user-dir))
(message "package-archives:\n%s"
         (mapconcat (lambda (cell)
                      (format "  %s: %s" (car cell) (cdr cell)))
                    package-archives "\n"))
(message "----------------------------------------------------------")
(message "Running package-lint on %s..." (string-join command-line-args-left " "))
(when (> (length command-line-args-left) 1)
  ;; Use `bound-and-true-p' to avoid an unbound variable error
  (message "package-lint-main-file: %s" (bound-and-true-p package-lint-main-file)))
(require 'cl-lib)
(defvar explicitly-installed-packages)
(when (boundp 'explicitly-installed-packages)
  (advice-add #'package-lint--check-packages-installable
              :filter-args
              (lambda (args)
                (list (cl-set-difference (car args) explicitly-installed-packages
                                         :test (lambda (x y) (eq (car x) y)))))))
(package-lint-batch-and-exit)

(provide 'package-lint-runner)
;;; package-lint-runner.el ends here

