#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun current-profile-list ()
  (parse-profile-string (env/list "CLPOM_PROFILE")))

(defun on-profile (string)
  (let ((profile (string-upcase string)))
    (not (null (find-if (lambda (x) (string= profile (string-upcase x))) (current-profile-list))))))
