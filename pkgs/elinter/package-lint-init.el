(require 'package)
(require 'lisp-mnt)

(push '(melpa . "https://melpa.org/packages/") package-archives)
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
