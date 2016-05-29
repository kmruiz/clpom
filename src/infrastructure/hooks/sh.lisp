#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun sh (command &rest arguments)
  (let ((cmd (apply 'format (concatenate 'list (list nil command) arguments))))
    (with-output-to-string (output) 
      (uiop:run-program cmd
			:ignore-error-status t
			:force-shell t
			:output output
			:error-output output))))
