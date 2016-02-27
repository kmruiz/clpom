#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(define-test task-must-run
  (let ((task (make-task "test")))
    (push-step-back task (make-task-step (lambda ($) (assert-equal t t))))
    (run-task task)))
