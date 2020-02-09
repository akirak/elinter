;; buttercup tests
(require 'hello)

(describe "example test group"
  (it "always succeed"
    (expect t :to-be t)))

(describe "hello-identity from hello.el"
  (it "returns the argument"
    (expect (hello-identity 'hello) :to-be 'hello)))
