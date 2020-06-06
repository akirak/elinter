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

(require 'subr-x)

;;;; Run package-lint
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

