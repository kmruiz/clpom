#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun merge-directory-in-java-classpath (classpath directory)
  (concatenate 'list
	       classpath
	       (loop for i in (directory (format nil "~a/**/*.jar" directory)) collect i)))

(defun classpath-string (classpath)
  (let ((cp (format nil "~{~a:~}" classpath)))
    (subseq cp 0 (1- (length cp)))))
