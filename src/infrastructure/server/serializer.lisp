#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun serialize-for-server (value &key (type "application/json") (stream *standard-output*))
  (cl-json:with-decoder-simple-clos-semantics
    (cond
      ((string= type "application/json") (cl-json:encode-json value stream))
      ((or (string= type "application/x-lisp") t) (write value :stream stream :pretty t)))))
