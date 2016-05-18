#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun call-project-generator (name project-name project-description)
  (cond
    ((string= name "lisp") (generate-lisp-project project-name project-description))
    (t (log-error "Invalid project generator ~a" name))))

(defun generate-project-in-place (name project-name project-description)
  (loop for (path code) in (call-project-generator name project-name project-description)
     do (progn
	  (log-info "Cooking file ~a" path)
	  (ensure-directories-exist path)
	  (when (probe-file path)
	    (log-warn "File ~a already exists, overwriting it..." path))
	  (with-open-file (stream path :direction :output
				  :if-does-not-exist :create
				  :if-exists :overwrite)
	    (cond
	      ((eql (first code) 'flatten-in)
	       (loop for i in (rest code) do
		    (progn
		      (write i :stream stream)
		      (format stream "~%"))))
	      (t 
	       (write code :stream stream)))))))

(defun run-generator (name)
  (log-info "We are going to generate a project of type '~a'" name)
  (log-info "But first I need to ask you few questions about it")
  (log-info "How is it called?")
  (let ((project-name (read-line)))
    (log-info "And a short description?")
    (let ((project-descr (read-line)))
      (log-info "OK. Wait a second while we generate the project structure.")
      (generate-project-in-place name project-name project-descr)
      (log-done "Happy hacking!"))))
