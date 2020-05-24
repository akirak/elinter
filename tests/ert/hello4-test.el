(require 'ert)
(require 'hello4)

(ert-deftest hello4-identity ()
  "Test if `hello4-identity' returns the right results."
  (should (= (hello4-identity "hi") "hi")))

(provide 'ert-test)
