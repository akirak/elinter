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
(cond
 ((eq system-type 'gnu/linux)
  (setq package-user-dir
        ;; If you want to store packages in a specific directory,
        ;; e.g. for caching on CI, you can give it as an environment
        ;; variable.
        (or (getenv "ELPA_USER_DIR")
            (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                                 (expand-file-name "~/.cache"))))
              (expand-file-name "melpa-check/elpa" xdg-cache)))))
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

(provide 'setup-package)
