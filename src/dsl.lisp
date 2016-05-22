#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defmacro defproject ((name) &body body)
  `(let ((project (make-project ,name)))
     (flet ((define (name value)
	      (put-extra project name value))
	    (task (&key name does dependencies)
	      (add-task project name does)
	      (loop for dep in dependencies do (add-task-dependency project name dep)))
	    (requires (&key task the-dependency)
	      (add-task-dependency project task the-dependency))
	    (include (&key dependency)
	      (ql:quickload dependency))
	    (plugin (plugin)
	      (register-plugin project plugin)))
       (macrolet ((on ((profile) &body body)
		    `(when (on-profile ,profile)
		       ,@body))
		  (deftask ((name) &body body)
		    `(task :name ,name :does (lambda (x) (declare (ignore x)) ,@body))))
	 ,@body))
     project))

