#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun load-tasks-from-directory (project directory)
  (loop for file in (directory (format nil "~a/**/*.*" directory) :resolve-symlinks t)
     for file-name = (namestring file)
     for index-of-dot = (position #\. file-name :from-end t :test #'equal)
     unless (null index-of-dot)
     do
       (let ((processed-file file) (extension (subseq file-name index-of-dot)))
	 (cond
	   ((string= extension ".sh")
	    (add-task project (pathname-name processed-file) "shell"
		      (lambda (x)
			(declare (ignore x))
			(log-info "Executing file ~a" (namestring processed-file))
			(format t "~a" (sh "sh ~a" (namestring processed-file))))))
	   ((string= extension ".lisp")
	    (with-open-file (stream processed-file)
	      (funcall (eval (read stream)) project)))))))

