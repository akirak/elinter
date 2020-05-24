(require 'ert)
(require 'hello3)

(ert-deftest hello3-identity ()
  "Test if `hello3-identity' returns the right results."
  (should (= (hello3-identity 1) 1))
  (should (equal (hello3-identity "hi") "hi")))

(provide 'ert-test)
