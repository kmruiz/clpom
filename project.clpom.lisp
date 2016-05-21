(defproject ("clpom")
  (plugin #'lisp-project)
  (define :version "1.1.0")
  (define :description "Basic Project Object Model management")
  (define :dendencies (list :lisp-unit :cl-ansi-text :cl-json))
  (task :name "clean" :does
	(lambda (x) (sh "rm -rf **/*~~")))
  (requires :task "update" :the-dependency "clean"))
