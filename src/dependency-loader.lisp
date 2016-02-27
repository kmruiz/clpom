#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun +ql-output+ () (make-broadcast-stream))

(defgeneric load-dependency (dependency))

(defmethod load-dependency ((dependency symbol))
  (log-info "Loading dependency ~a" dependency)
  (with-open-stream (*standard-output* (+ql-output+))
    (ql:quickload dependency)))
