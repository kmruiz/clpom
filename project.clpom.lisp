(defproject ("clpom")
  (plugin #'lisp-project)
  (define :version "1.1.0")
  (define :description "Basic Project Object Model management")
  (define :dependencies (list :lisp-unit :cl-ansi-text :cl-json :split-sequence :drakma :hunchentoot))
  (deftask ("clean")
      (sh "rm -rf **/*~~"))
  (deftask ("welcome")
    (on ("production") (log-info "Welcome to a production environment"))
    (on ("test") (log-info "Welcome to a test environment")))
  (requires :task "update" :the-dependency "clean"))
