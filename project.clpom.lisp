(defproject ("clpom")
  (plugin #'lisp-project)
  (define :version "0.1.0")
  (define :description
      "Basic Project Object Model management"
    )
  (define :dependencies '(:lisp-unit :cl-ansi-text)))