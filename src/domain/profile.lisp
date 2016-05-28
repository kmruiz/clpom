#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun current-profile-list ()
  (parse-profile-string (sb-ext:posix-getenv "CLPOM_PROFILE")))

(defun parse-profile-string (string)
  (split-sequence:split-sequence "," string :test 'string=))

(defun on-profile (string)
  (let ((profile (string-upcase string)))
    (not (null (find-if (lambda (x) (string= profile (string-upcase x))) (current-profile-list))))))
