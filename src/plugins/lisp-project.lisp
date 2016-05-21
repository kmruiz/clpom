#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun run-tests! (project-name)
  (use-debugger :ask)
  (let ((start-time (get-internal-run-time)))
    (let* ((r (with-output-supression (run-tests :all project-name)))
	   (error-count (+ (or (length (failed-tests r)) 0) (or (length (error-tests r)) 0))))
      (let ((end-time (get-internal-run-time)))
	(write-tap-to-file r "build/test-results.tap")
	(log-info "Ran ~d test~:p in ~f second~:p"
		  (length (test-names r)) (/ (- end-time start-time) internal-time-units-per-second))
	(cond
	  ((> error-count 0)
	   (log-error "~d test~:p failed" error-count)
	   (print-failures r)
	   (print-errors r)
	   nil)
	  (t t))))))

(defun lisp-project (project)
  (let ((project-name (intern (string-upcase (name project)))))
    (add-task project "clean"
	      (lambda ($)
		(when (probe-file "build")
		  (sb-ext:delete-directory "build" :recursive t))))
    (add-task project "update"
	      (lambda ($)
		(let ((deps (get-extra project :dependencies)))
		  (format t "deps ~a" deps)
		  (loop for i in deps do (load-dependency i)))))
    (add-task project "upgrade"
	      (lambda ($)
		(with-output-supression
		  (ql:update-all-dists))))
    (add-task project "load-system"
	      (lambda ($)
		(load (format nil "~a.asd" (name project)) :verbose nil :print nil)
		(asdf:load-system project-name)))
    (add-task project "test"
	      (lambda ($)
		(let ((d (make-pathname :directory '(:relative "test") :name :wild :type "lisp")))
		  (mapcar (lambda (x) (unless (search "runner" (namestring x)) (load x))) (directory d))
		  (let ((r (run-tests! project-name)))
		    (cond
		      ((null r)
		       (sb-ext:exit :code 1))
		      (t t))))))
    (add-task project "dist"
	      (lambda ($)
		(let ((version (or (get-extra project :version) "default")))
		  (ensure-directories-exist (format nil "build/~a/" version))
		  (let ((pid (sb-posix:fork)))
		    (cond
		      ((zerop pid) 
		       (sb-ext:save-lisp-and-die
			(format nil "build/~a/~a" version (name project))
			:executable t
			:toplevel (find-symbol (string-upcase
						(or (get-extra project :main-function) "main")) project-name)
			:compression t
			:purify t))
		      ((plusp pid)
		       (sb-posix:waitpid pid 0)))))))
    (add-task project "help"
	      (lambda ($)
		(log-info "~a:~a task list" (name project) (get-extra project :version))
		(log-info "~a" (get-extra project :description))
		(loop
		   for i in (sort (copy-list (map 'list #'name (tasks project))) #'string<)
		   do (format t "~2a~a~%" "" i)))))

  (add-task-dependency project "dist" "test")
  (add-task-dependency project "test" "load-system")
  (add-task-dependency project "load-system" "update"))
