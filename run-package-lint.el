;; The logic is based on an implementation in makel:
;; <https://gitlab.petton.fr/DamienCassou/makel/blob/master/makel.mk>

;;;; Configure package.el
(setq load-prefer-newer t)
(require 'package)
;; Configure the standard set of package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Set the package cache location to the environment variable
;; You need to provide this environment variable from the outside script.
(setq package-user-dir (getenv "ELPA_USER_DIR"))
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
(package-lint-batch-and-exit)
