#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun generate-package (name)
  (let ((pkg-name (intern (string-upcase name))))
    `(defpackage ,pkg-name
       (:use :cl :lisp-unit))))

(defun generate-cl-project (name description)
  `(defproject (,name)
       (plugin #'lisp-project)
     (define :version "0.0.1")
     (define :description ,description)
     (define :dependencies '(:lisp-unit))))

(defun generate-asdf-file (name description)
  (let ((pkg-name (intern (string-upcase (format nil "~a-asd" name)))))
    `(flatten-in
      (defpackage ,pkg-name
	(:use :cl :asdf))

      (in-package ,pkg-name)

      (defsystem ,name
	:name ,name
	:version "0.0.1"
	:maintainer "Someone"
	:author "Someone <someone@clpom.com>"
	:licence "Apache2 License (view LICENSE for more info)"
	:description ,description
	:serial t
	:components ((:file "src/package")
		     (:file "src/main"))
	:depends-on ("lisp-unit")))))

(defun generate-src/main.lisp (name)
  (let ((pkg-name (intern (string-upcase name))))
    `(flatten-in
      (in-package ,pkg-name)
      (defun main ()
	(format t "Hello World~%")))))

(defun generate-test/sample-spec.lisp (name)
  (let ((pkg-name (intern (string-upcase name))))
    `(flatten-in
      (in-package ,pkg-name)
      (define-test check-environment
	  (assert-equal 0 0)
	(assert-equal "X" "X")
	(assert-equal t t)
	(assert-equal 'a 'a)))))

(defun generate-lisp-project (name description)
  `(("project.clpom.lisp" ,(generate-cl-project name description))
    (,(format nil "~a.asd" name) ,(generate-asdf-file name description))
    ("src/main.lisp" ,(generate-src/main.lisp name))
    ("test/sample-spec.lisp" ,(generate-test/sample-spec.lisp name))
    ("src/package.lisp" ,(generate-package name))))
