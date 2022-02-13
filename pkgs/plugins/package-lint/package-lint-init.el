(setq package-user-dir (locate-user-emacs-file
                        (format "elpa/%d" emacs-major-version)))
(require 'package)
(require 'lisp-mnt)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)
(package-initialize)

(defvar package-lint-main-file)

(let ((required-packages
       (with-temp-buffer
         (insert-file-contents package-lint-main-file)
         (emacs-lisp-mode)
         (mapcar #'car (read (car (lm-header-multiline "Package-Requires")))))))
  (unless (cl-reduce (lambda (acc package)
                       (and acc
                            (or (package-installed-p package)
                                (assq package package-archive-contents))))
                     required-packages
                     :initial-value t)
    (package-refresh-contents))
  (dolist (package required-packages)
    (unless (package-installed-p package)
      (package-install package))))
