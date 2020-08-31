;; This file is loaded by default when elinter performs static checks
;; on Emacs Lisp source files.

;; These settings are from melpazoid.el.
;; https://github.com/riscy/melpazoid/blob/master/melpazoid/melpazoid.el

;; These settings makes the checkdoc checks less strict.
(setq sentence-end-double-space nil
      checkdoc-proper-noun-list nil
      checkdoc-common-verbs-wrong-voice nil)
