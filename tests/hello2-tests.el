;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'hello2)

(describe "test group for hello2"
  (it "always succeed"
    (expect t :to-be t)))

(provide 'hello2-tests)
