;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'hello2)

(ert-deftest hello2-always-success ()
  (should (= (1+ 1) 2)))

(ert-deftest hello2-identity ()
  (should (= (hello-identity 1) 1)))

(provide 'hello2-ert-tests)
