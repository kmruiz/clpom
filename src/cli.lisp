#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun file-string (path)
  (when (probe-file path)
    (with-open-file (stream path)
      (let ((data (make-string (file-length stream))))
	(read-sequence data stream)
	data))))

(defmacro with-project-definition ((project &key at-file) &body body)
  `(let ((project-repr (file-string ,at-file)))
     (cond
       ((null project-repr)
	(log-error "Could not find file named ~a" ,at-file))
       (t
	(let ((,project (eval (read-from-string project-repr))))
	  (load-tasks-from-directory ,project "project")
	  ,@body)))))

(defun do-tasks-at-file (file args)
  (with-project-definition (project :at-file file)
    (log-info "Current project ~a" (%wrap-on-color :cyan (name project)))
    (loop for i in args do (run-project-task project i))))

(defun main ()
  (require :sb-posix)
  (in-package #:clpom)

  (progn
    (let ((start-time (get-internal-run-time))
	  (args (rest sb-ext:*posix-argv*)))
      (cond
	((string= (first args) "--register-autocomplete")
	 (register-autocomplete))
	((string= (first args) "--autocomplete")
	 (with-project-definition (project :at-file "project.clpom.lisp")
	   (format t "~{~a ~}~%" (match-all-tasks-for-project project (second args)))))
	((string= (first args) "--server")
	 (with-project-definition (project :at-file "project.clpom.lisp")
	   (start-server project)))
	((string= (first args) "--init")
	 (run-generator (second args)))
	((string= (first args) "--install")
	 (install-dependency (intern (string-upcase (format nil "~a" (second args))) :keyword)))
	(t 
	 (do-tasks-at-file "project.clpom.lisp" args)
	 (let ((end-time (get-internal-run-time)))
	   (log-done "Everything in ~f second~:p" (/ (- end-time start-time) internal-time-units-per-second))))))))
