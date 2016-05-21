(defproject ("clpom")
  (plugin #'lisp-project)
  (define :version "1.1.0")
  (define :description "Basic Project Object Model management")
  (define :dependencies (list :lisp-unit :cl-ansi-text :cl-json :split-sequence :drakma :hunchentoot))
  (task :name "clean" :does
	(lambda (x) (sh "rm -rf **/*~~")))
  (requires :task "update" :the-dependency "clean"))
