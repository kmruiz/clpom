#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun %group-by-category (tasks)
  (let (keys backlog)
    (loop for i in tasks
       do (push (category i) keys)
       do (push i (getf backlog (category i))))
    (loop for i in (remove-duplicates keys :test #'string=) collect (list i (getf backlog i)))))

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
	 (add-task project "help" "tools"
		   (lambda ($)
		     (declare (ignore $))
		     (log-info "Version ~a" (get-extra project :version))
		     (log-info "~a" (get-extra project :description))
		     (loop
			for group in (%group-by-category (tasks project))
			do (progn
			     (log-info "****** ~24a ******" (%wrap-on-color :yellow (first group)))
			     (loop for task in
				  (sort (copy-list (map 'list #'name (second group))) #'string<)
				do (log-info "~31a ******" (%wrap-on-color :magenta task)))))))
	 ,@body))
     project))


(defparameter sample
  (list
   (make-task "1" :category "number")
   (make-task "2" :category "number")
   (make-task "a" :category "letter")
   (make-task "b" :category "letter")
   (make-task "1a" :category "mixed")))
