#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(define-test sh-echo-should-print-output
    (assert-equal (format nil "Hi~%") (sh "echo ~a" "Hi")))

(define-test sh-cat-number-should-print-number
    (assert-false (string= "123456789" (sh "cat test/resources/number"))))
