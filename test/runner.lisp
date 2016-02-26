(in-package :clpom)

(define-test check-environment
  (assert-equal 0 0)
  (assert-equal "X" "X")
  (assert-equal nil nil)
  (assert-equal 'a 'a))

(defun run-tests! ()
  (setq *print-failures* t)
  (let ((r (run-tests :all :clpom)))
    (cond
      ((or (> (length (failed-tests r)) 0) (> (length (error-tests r)) 0))
       (print-errors r)
       nil)
      (t t))))
