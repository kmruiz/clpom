#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defparameter *acceptor* "none")

(defun accepts* ()
  (or (hunchentoot:header-in* :accept) "application/x-lisp"))

(defun %answer (value)
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (stream)
    (serialize-for-server value :type (accepts*) :stream stream)))

(defun %cons-pairs (list)
  (unless (null list)
    (concatenate 'list
		 (list (cons (car list) (cadr list)))
		 (%cons-pairs (cddr list)))))

(defun stop-server ()
  (hunchentoot:stop *acceptor*))

(defun start-server (project &optional (port 1325))
  (hunchentoot:define-easy-handler (tasks-of-project :uri "/tasks") ()
    (%answer (sort (copy-list (map 'list #'name (tasks project))) #'string<)))

  (hunchentoot:define-easy-handler (project-extra-list :uri "/extras") ()
    (%answer (%cons-pairs (extras project))))

  (hunchentoot:define-easy-handler (task-execution :uri "/task") (name)
    (%answer (split-sequence:split-sequence #\Newline (sh "clpom ~a" name))))

  (hunchentoot:define-easy-handler (kill-server :uri "/bye") ()
    (stop-server))
  
  (setq *acceptor*
	(hunchentoot:start (make-instance
			    'hunchentoot:easy-acceptor
			    :port port			  
			    :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)))))

