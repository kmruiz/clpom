#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defparameter +dtl-directory+ "test/resources/dynamic-task-loader")

(define-test dynamic-task-loader-should-load-tasks-as-shell-scripts
    (let ((project (make-project "test")) called)
      (load-tasks-from-directory project +dtl-directory+)
      (run-project-task project "hi")))

(define-test dynamic-task-loader-should-load-lisp-files
    (let ((project (make-project "test")))
      (load-tasks-from-directory project +dtl-directory+)
      (assert-true (get-extra project :called))))
