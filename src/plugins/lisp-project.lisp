#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun lisp-project (project)
  (require :asdf)

  (let ((project-name (intern (string-upcase (name project)))))
    (add-task project "update"
	      (lambda ($)
		(let ((deps (get-extra project :dependencies)))
		  (loop for i in deps do (ql:quickload i)))))
    (add-task project "load-system"
	      (lambda ($)
		(load (format nil "~a.asd" (name project)) :verbose nil :print nil)
		(asdf:load-system project-name)))
    (add-task project "test"
	      (lambda ($)
		(let ((runner (or (get-extra project :lispunit-runner) "test/runner.lisp")))
		  (when (probe-file runner)
		    (load runner)
		    (let ((d (make-pathname :directory '(:relative "test") :name :wild :type "lisp")))
		      (mapcar (lambda (x) (unless (search "runner" (namestring x)) (load x))) (directory d))
		      (funcall (symbol-function (find-symbol "RUN-TESTS!" project-name))))))))
    (add-task project "dist"
	      (lambda ($)
		(let ((version (or (get-extra project :version) "default")))
		  (ensure-directories-exist (format nil "build/~a/" version))
		  (sb-ext:save-lisp-and-die
		   (format nil "build/~a/~a" version (name project))
		   :executable t
		   :toplevel (find-symbol (string-upcase (or (get-extra project :main-function) "main")) project-name)
		   :compression t)))))

  (add-task-dependency project "dist" "test")
  (add-task-dependency project "test" "load-system")
  (add-task-dependency project "load-system" "update"))
