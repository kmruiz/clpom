(defpackage #:clpom-asd
  (:use :cl :asdf))

(in-package :clpom-asd)

(defsystem clpom
  :name "clpom"
  :version "0.0.1"
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
               (:file "src/cli"))
  :depends-on ("quicklisp" "lisp-unit" "cl-ansi-text"))
