#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun generate-java-clpom-project (name description)
  `(defproject (,name)
       (plugin #'java-project)
     (define :version "0.0.1")
     (define :description ,description)
     (define :dependencies '(:lisp-unit))))

(defun generate-java-boot (name)
  (format nil
	  "package com.~a;

public class Boot {
  public static void main(String[] args) {
    System.out.println(~aHello ~a~a);
  }
}" name #\" name #\"))

(defun generate-java-project (name description)
  `((,(format nil "src/main/java/com/~a/Boot.java" name) ,(generate-java-boot name))
    ("project.clpom.lisp" ,(generate-java-clpom-project name description))))
