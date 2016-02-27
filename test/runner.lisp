#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(define-condition tests-failed (error)
  ((count :initarg :count :reader error-count)))

(define-test check-environment
  (assert-equal 0 0)
  (assert-equal "X" "X")
  (assert-equal nil nil)
  (assert-equal 'a 'a))


(defmacro with-output-supression (&body body)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     (with-open-stream (*trace-output* (make-broadcast-stream))
       ,@body)))

(defun run-tests! ()
  (use-debugger :ask)
  (let ((start-time (get-internal-run-time)))
    (let* ((r (with-output-supression (run-tests :all :clpom)))
	   (error-count (+ (or (length (failed-tests r)) 0) (or (length (error-tests r)) 0))))
      (let ((end-time (get-internal-run-time)))
	(write-tap-to-file r "build/test-results.tap")
	(log-info "Ran ~d test~:p in ~f second~:p"
		  (length (test-names r)) (/ (- end-time start-time) internal-time-units-per-second))
	(cond
	  ((> error-count 0)
	   (log-error "~d test~:p failed" error-count)
	   (print-errors r)
	   (loop
	      for i from 0
	      for e in (append (failed-tests r) (error-tests r))
	      do (log-error "~d: ~a" i (format nil "~a" e)))
	   nil)
	  (t t))))))
