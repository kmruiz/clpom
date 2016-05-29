#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun %string-replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun %is-junit (pathname)
  (search "junit" (namestring pathname)))

(defun %class-for-file (base-dir full-file)
  (let ((file (namestring full-file)))
    (let* ((fn-start (+ (search base-dir file) (length base-dir)))
	   (fn-end (search ".java" file))
	   (relative (subseq file fn-start fn-end)))
      (%string-replace-all relative "/" "."))))

(defun %to-java-class (from-base)
  (lambda (file)
    (%class-for-file from-base file)))

(defun find-test-classes (directory)
  (mapcar (%to-java-class directory) (directory (format nil "~a/**/*.java" directory))))

(defun run-tests-using-classpath (classpath test-names)
  (cond
    ((find-if #'%is-junit classpath)
     (sh "java -cp ~a org.junit.runner.JUnitCore ~{~a ~}" (classpath-string classpath) test-names))))
