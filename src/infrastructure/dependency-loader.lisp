#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun +ql-output+ () (make-broadcast-stream))

(defgeneric load-dependency (dependency))
(defgeneric install-dependency (dependency))

(defmethod load-dependency ((dependency symbol))
  (log-trace "Loading dependency ~a" (%wrap-on-color :cyan (string-downcase dependency)))
  (with-open-stream (*standard-output* (+ql-output+))
    (ql:quickload dependency)))

(defmethod install-dependency ((dependency symbol))
  (log-info "Installing dependency ~a" (%wrap-on-color :cyan (string-downcase dependency)))
  (let ((new-project-descriptor
	 (with-open-file (stream "project.clpom.lisp")
	   (let ((all (read stream)))
	     (map 'list
	      (lambda (x)
		(cond
		  ((and
		    (listp x)
		    (eql (first x) 'define)
		    (eql (second x) :dependencies))
		   (list 'define :dependencies
			 `,(remove-duplicates
			    (concatenate 'list '(list) (second (third x)) (list dependency)))))
		  (t x)))
	      all)))))
    (with-open-file (stream "project.clpom.lisp" :if-exists :overwrite :direction :output)
      (write new-project-descriptor :stream stream :pretty t))))

