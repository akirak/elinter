;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((purescript-mode
  (mode . purty-on-save))
 (nil
  (projectile-project-compilation-cmd . "spago bundle-app --no-install --no-build --to dist.js")))
