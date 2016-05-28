#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(define-test project-must-put-extra
    (let ((project (make-project "test")))
      (put-extra project :some-extra 1)
      (assert-equal 1 (get-extra project :some-extra))))

(define-test project-must-run-registered-task
    (let ((project (make-project "test")) called)
      (add-task project "test" "checks"
		(lambda (x)
		  (declare (ignore x))
		  (setq called t)))
      (run-project-task project "test")
      (assert-true called)))


(define-test project-must-apply-plugin
    (let* ((project (make-project "test"))
	   (plugin (lambda (project) (put-extra project :called t))))
      (register-plugin project plugin)
      (assert-true (get-extra project :called))))
