;; The logic is based on an implementation in makel:
;; <https://gitlab.petton.fr/DamienCassou/makel/blob/master/makel.mk>

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
              (expand-file-name "melpa-check/elpa" xdg-cache))))
  (message "Set package-user-dir to %s" package-user-dir))

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
(message "Running package-lint on %s..." command-line-args-left)
(require 'cl-lib)
(defvar explicitly-installed-packages)
(when (boundp 'explicitly-installed-packages)
  (advice-add #'package-lint--check-packages-installable
              :filter-args
              (lambda (args)
                (list (cl-set-difference (car args) explicitly-installed-packages
                                         :test (lambda (x y) (eq (car x) y)))))))
(package-lint-batch-and-exit)
