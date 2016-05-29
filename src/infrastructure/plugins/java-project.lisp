#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defparameter +java-kw+
  (list "abstract" "assert" "boolean" "break" "byte" "case" "catch" "char" "class" "const"
	"continue" "default" "do" "double" "else" "enum" "extends" "final" "finally" "float"
	"for" "goto" "if" "implements" "import" "instanceof" "int" "interface" "log" "native"
	"new" "package" "private" "protected" "public" "return" "short" "static" "strictfp"
	"super" "switch" "synchronized" "this" "thow" "throws" "transient" "try" "void"
	"volatile" "while"))

(defparameter +java-important-types+
  (list "ArrayList" "LinkedList" "List" "Integer" "Boolean" "Double" "Float" "String"
	"Character" "Byte"))

(defparameter %kw-color :magenta)
(defparameter %type-color :yellow)
(defparameter %file-color :blue)
(defparameter %line-color :cyan)

(defun %path-relative-to (base-dir full-file)
  (let ((file (namestring full-file)))
    (let ((fn-start (+ (search base-dir file) (length base-dir))))
      (subseq file fn-start))))

(defun %format-javac-output (output)
  (let ((input (make-string-input-stream output)))
    (with-output-to-string (stream)
      (loop for line = (read-line input nil)
	 while line
	 do
	   (cond
	     ((string= line "/" :end1 1)
	      (let* ((filename-end (search ":" line))
		     (line-number-end (search ":" line :start2 (1+ filename-end)))
		     (filename (subseq line 0 filename-end))
		     (line-number (subseq line (1+ filename-end) line-number-end)))
		(format stream "~a~%"
			(%string-replace-all
			 (%string-replace-all
			  (%string-replace-all
			   (%string-replace-all
			    line line-number (%wrap-on-color %line-color line-number))
			   "errors" (%wrap-on-color :red "errors"))
			  filename (%wrap-on-color %file-color filename))
			 "error" (%wrap-on-color :red "error")))))
	     ((cl-ppcre:all-matches "\\d errors?" line)
	      (let ((number-style (cl-ansi-text:make-color-string :red :effect :italic))
		    (err-count (parse-integer line :junk-allowed t)))
		(format stream "~a~a~a error~p~%"
			number-style err-count cl-ansi-text:+reset-color-string+ err-count)))
	     (t
	      (format stream "~a~%"
		      (reduce (lambda (string kw)
				(%string-replace-all string kw (%wrap-on-color %kw-color kw)))
			      +java-kw+
			      :initial-value 
			      (reduce
			       (lambda (string type)
				 (%string-replace-all string type (%wrap-on-color %type-color type)))
			       +java-important-types+
			       :initial-value
			       (%string-replace-all
				(%string-replace-all line "errors" (%wrap-on-color :red "errors"))
				"error" (%wrap-on-color :red "error")))))))))))

(defun jar-folder (folder pathname)
  (zip:zip pathname folder))

(defun java-project (project)
  (add-task project "clean" "tools"
	    (lambda ($)
	      (when (probe-file "build")
		(sb-ext:delete-directory "build" :recursive t))))

  (add-task project "compile" "build"
	    (lambda ($)
	      (ensure-directories-exist "build/compile/")
	      (let ((compiler-output
		     (sh "javac -d build/compile/ ~{~a ~}" (directory "src/main/java/**/*.java"))))
		(when (search "error" compiler-output)
		  (log-error "Could not compile classes. There are compiler errors")
		  (format t "~a" (%format-javac-output compiler-output))))))
  (add-task project "compile-test" "build"
	    (lambda ($)
	      (ensure-directories-exist "build/test-compile/")
	      (let* ((classpath (merge-directory-in-java-classpath
				 (list "build/compile" "build/test-compile")
				 ".clpom/library/"))
		     (javac-command
		      (format nil
			      "javac -cp ~a -d build/test-compile/ ~{~a ~}"
			      (classpath-string classpath)
			      (directory "src/test/java/**/*.java")))
		     (compiler-output
		      (progn
			(log-trace "Using javac command ~a" javac-command)
			(sh javac-command))))
		(when (search "error" compiler-output)
		  (log-error "Could not compile test classes. There are compiler errors")
		  (format t "~a" (%format-javac-output compiler-output))))))
  (add-task project "find-main-class" "build"
	    (lambda ($)
	      (let (main-class)
		(block loop
		  (loop for i in (directory "src/main/java/**/*.java")
		     do
		       (with-open-file (stream i)
			 (let* ((stream-length (file-length stream))
				(data (make-string stream-length)))
			   (read-sequence data stream)
			   (when (search "public static void main" data)
			     (setq main-class (%class-for-file "src/main/java/" i))
			     (return-from loop))))))
		(ensure-directories-exist "build/compile/META-INF/MANIFEST.MF")
		(cond
		  (main-class 
		   (with-open-file (stream "build/compile/META-INF/MANIFEST.MF"
					   :direction :output
					   :if-exists :append
					   :if-does-not-exist :create)
		     (log-trace "Found main class ~a" main-class)
		     (format stream "Manifest-Version: 1.0~%")
		     (format stream "Created-By: clpom-~a~%" (name project))
		     (format stream "Main-Class: ~a~%" main-class)))
		  (t (log-warn "Could not find main class. Assuming it's a library."))))))
  (add-task project "jar" "distribution"
	    (lambda ($)
	      (let ((output-jar (format nil "build/jar/~a-~a.jar" (name project) (get-extra project :version))))
		(ensure-directories-exist output-jar)
		(jar-folder "build/compile/" output-jar))))
  (add-task project "jar-with-sources" "distribution"
	    (lambda ($)
	      (let ((output-jar (format nil "build/jar/~a-~a-sources.jar" (name project) (get-extra project :version))))
		(ensure-directories-exist output-jar)
		(log-trace "Generating sources jar for src/main/java in ~a" output-jar)
		(jar-folder "src/main/java/" output-jar))))

  (add-task project "run-jar" "other"
	    (lambda ($)
	      (let ((output-jar (format nil "build/jar/~a-~a.jar" (name project) (get-extra project :version))))
		(log-trace "Running jar ~a" output-jar)
		(format t "~a" (sh "java -jar ~a" output-jar)))))

  (add-task project "test" "check"
	    (lambda ($)
	      (let ((test-classes (find-test-classes "src/test/java/")))
		(log-trace (format nil "Running test classes~%~{~a~%~}" test-classes))
		(let ((output
		       (run-tests-using-classpath
			(merge-directory-in-java-classpath (list "build/compile" "build/test-compile") ".clpom/library/")
			test-classes)))
		  (cond
		    ((search "FAILURES!!!" output)
		     (log-error "There were test failures!")
		     (format t "~a" output)
		     (error 'simple-error))
		    (t))))))
  
  (add-task project "dist" "distribution" (lambda ($)))
  
  (add-task-dependency project "run-jar" "jar")

  (add-task-dependency project "dist" "jar")
  (add-task-dependency project "dist" "jar-with-sources")

  (add-task-dependency project "jar" "find-main-class")
  (add-task-dependency project "jar" "test")
  
  (add-task-dependency project "jar-with-sources" "jar")
  (add-task-dependency project "find-main-class" "test")

  (add-task-dependency project "test" "compile-test")
  (add-task-dependency project "compile-test" "compile")
  (add-task-dependency project "compile" "clean"))
