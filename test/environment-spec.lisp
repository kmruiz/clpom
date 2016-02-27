#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)


(define-test check-environment
  (assert-equal 0 0)
  (assert-equal "X" "X")
  (assert-equal t t)
  (assert-equal 'a 'a))
