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
		  (%wrap-on-color :green (length (test-names r)))
		  (/ (- end-time start-time) internal-time-units-per-second))
	(cond
	  ((> error-count 0)
	   (log-error "~d test~p failed" (%wrap-on-color :red error-count) error-count)
	   (print-failures r)
	   (print-errors r)
	   nil)
	  (t t))))))

(defun lisp-project (project)
  (let ((project-name (intern (string-upcase (name project)))))
    (add-task project "clean" "tools"
	      (lambda ($)
		(when (probe-file "build")
		  (sb-ext:delete-directory "build" :recursive t))))
    (add-task project "update" "dependencies"
	      (lambda ($)
		(let ((deps (get-extra project :dependencies)))
		  (loop for i in deps do (load-dependency i)))))
    (add-task project "upgrade" "dependencies"
	      (lambda ($)
		(with-output-supression
		  (ql:update-all-dists))))
    (add-task project "load-system" "tools"
	      (lambda ($)
		(load (format nil "~a.asd" (name project)) :verbose nil :print nil)
		(asdf:load-system project-name)))
    (add-task project "test" "checks"
	      (lambda ($)
		(let ((d (format nil "test/**/*.lisp")))
		  (mapcar (lambda (x)
			    (log-trace "Iterating on file ~a" x)
			    (unless (search "runner" (namestring x)) (load x))) (directory d))
		  (let ((r (run-tests! project-name)))
		    (cond
		      ((null r)
		       (sb-ext:exit :code 1))
		      (t t))))))
    (add-task project "dist" "distribution"
	      (lambda ($)
		(let ((version (or (get-extra project :version) "default")))
		  (ensure-directories-exist (format nil "build/~a/" version))
		  (let ((pid (sb-posix:fork)))
		    (cond
		      ((zerop pid)
		       (let ((toplevel (find-symbol
					(string-upcase
					 (or (get-extra project :main-function) "main")) project-name)))
			 (sb-ext:save-lisp-and-die
			  (format nil "build/~a/~a" version (name project))
			  :executable t
			  :toplevel toplevel
			  :root-structures (list toplevel)
			  :compression t
			  :purify t)))
		      ((plusp pid)
		       (sb-posix:waitpid pid 0))))))))

  (add-task-dependency project "dist" "test")
  (add-task-dependency project "test" "load-system")
  (add-task-dependency project "load-system" "update"))
