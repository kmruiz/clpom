#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(define-test task-must-run
  (let ((task (make-task "test")) called)
    (push-step-back task (make-task-step (lambda ($) (setq called t))))
    (run-task task)
    (assert-true called)))

(define-test task-must-stack
    (let* ((called-count 0) (task (make-task "inc"))
	  (%inc-call-count
	   (lambda (x)
	     (declare (ignore x))
	     (setq called-count (1+ called-count)))))
      (loop for i from 0 to 9
	 do (push-step-back task (make-task-step %inc-call-count)))
      (run-task task)
      (assert-equal 10 called-count)))

(define-test task-must-call-dependents
    (let* ((test (make-task "test"))
	   (clean (make-task "clean"))
	   (called-tasks nil)
	   (gen-task
	    (lambda (name)
	      (lambda (x)
		(declare (ignore x))
		(push name called-tasks)))))
      (push-step-back test (make-task-step (funcall gen-task "test")))
      (push-step-back clean (make-task-step (funcall gen-task "clean")))
      
      (task-depends-on test clean)
      (run-task test)
      (assert-equal (list "test" "clean") called-tasks)))
