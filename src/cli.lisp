#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun main ()
  (require :sb-posix)
  (in-package #:clpom)

  (let ((args (rest sb-ext:*posix-argv*)))
    (let ((project (eval (read-from-string (file-string "project.clpom.lisp")))))
      (loop for i in args do (run-project-task project i)))))
