(unless (file-directory-p user-emacs-directory)
  (make-directory user-emacs-directory t))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(let ((pkg-file (car (directory-files default-directory nil (rx "-pkg.el" eol)))))
  (if pkg-file
      (let* ((deps (with-temp-buffer
                     (insert-file-contents pkg-file)
                     (goto-char (point-min))
                     (cadr (nth 4 (read (current-buffer))))))
             (min-emacs-version (nth 1 (assoc 'emacs deps)))
             (packages (cl-remove-if (lambda (cell)
                                       (eq 'emacs (car cell)))
                                     deps)))
        (when (version< emacs-version min-emacs-version)
          (error "This package requires Emacs %s, but the current version is %s"
                 min-emacs-version emacs-version)
          (kill-emacs 1))
        (dolist (package (mapcar #'car packages))
          (unless (package-installed-p package)
            (package-install package))))))
