;;; melpazoid-misc-runner.el --- Run extra checks by melpazoid -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (melpazoid "0"))
;; Keywords: lisp maint
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

;; This file provides a function for running misc checks from
;; melpazoid in batch mode.

;;; Code:

(require 'melpazoid)
(require 'thingatpt)
(require 'subr-x)

(defun melpazoid-misc-runner-batch ()
  "Perform extra static checks using melpazoid."
  (message "Running the following checks from melpazoid.el:")
  (message "- melpazoid-check-sharp-quotes")
  (message "- melpazoid-check-misc")
  (message "----------------------------------------------------------")
  (message "Running checks on %s..." (string-join command-line-args-left " "))
  ;; Prevent printing the header
  (setq melpazoid--misc-header-printed-p t)
  (let (file has-errors)
    (while (setq file (pop command-line-args-left))
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
            (with-current-buffer err-buf
              (goto-char (point-min))
              (while (and (< (point) (point-max))
                          (looking-at (rx (+ anything))))
                (let* ((err (substring (thing-at-point 'line t) 2))
                       (loc (and (string-match (rx "#L" (group (+ digit))
                                                   ": ")
                                               err)
                                 (read (match-string 1 err))))
                       (source (with-current-buffer file-buf
                                 (forward-line (- loc
                                                  (line-number-at-pos)))
                                 (string-trim-right
                                  (thing-at-point 'line t)))))
                  (message "\n%s> %s" err source)
                  (setq has-errors t))
                (forward-line)))))))
    (kill-emacs (when has-errors 1))))

(provide 'melpazoid-misc-runner)
;;; melpazoid-misc-runner.el ends here
