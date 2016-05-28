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
  (assert-equal 0 0))
