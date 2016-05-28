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
               (:file "src/infrastructure/environment")
	       (:file "src/infrastructure/logger")
               (:file "src/domain/task")
               (:file "src/domain/project")
	       (:file "src/infrastructure/dependency-loader")
               (:file "src/infrastructure/plugins/lisp-project")
	       (:file "src/generator/cl-project")
	       (:file "src/generator/generator")
	       (:file "src/infrastructure/shell/profile")
	       (:file "src/infrastructure/shell/autocomplete")
	       (:file "src/infrastructure/hooks/sh")
	       (:file "src/infrastructure/hooks/dynamic-task-loader")
	       (:file "src/infrastructure/server/serializer")
	       (:file "src/infrastructure/server/hunchentoot")
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
