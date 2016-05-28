#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(macrolet ((define-logtest (name expected-level)
	     `(define-test ,(intern (string-upcase
				    (format nil "log-level-for-~a-must-be-~a" name expected-level)))
		  (assert-equal ,expected-level (%log-level ,name)))))
  (define-logtest "trace" 1)
  (define-logtest "info" 2)
  (define-logtest "warn" 3)
  (define-logtest "error" 4)
  (define-logtest "done" 5))

(define-test log-must-not-print-when-lower-level
    (assert-false (%should-print "trace" :min-log "error")))

