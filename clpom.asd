(defpackage #:clpom-asd
  (:use :cl :asdf))

(in-package :clpom-asd)

(defsystem clpom
  :name "clpom"
  :version "2.0.0"
  :maintainer "Kevin Mas Ruiz"
  :author "Kevin Mas Ruiz <masruizkevin@gmail.com>"
  :licence "Apache2 License (view LICENSE for more info)"
  :description "A project manager for Common Lisp"
  :serial t
  :components ((:file "src/package")
	       (:file "src/logger")
	       (:file "src/dependency-loader")
               (:file "src/task")
               (:file "src/project")
               (:file "src/plugins/lisp-project")
               (:file "src/dsl")
	       (:file "src/init/cl-project")
	       (:file "src/generator")
	       (:file "src/profile")
	       (:file "src/shell/autocomplete")
	       (:file "src/plugins/sh")
	       (:file "src/plugins/dynamic-task-loader")
	       (:file "src/server/serializer")
	       (:file "src/server/hunchentoot")
	       (:file "src/cli"))
  :depends-on
  ("quicklisp"
   "lisp-unit"
   "cl-ansi-text"
   "cl-json"
   "hunchentoot"
   "xmls"
   "drakma"
   "split-sequence"))
