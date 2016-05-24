#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

(defun match-all-tasks-for-project (project concept)
  (loop for i in (mapcar #'name (tasks project))
     when (search concept i)
     collect i))

(defun register-autocomplete ()
  (with-open-file (stream "/etc/bash_completion.d/clpom" :direction :output)
    (format stream "~{~a~%~}"
	    (list
	     "_clpom()"
	     "{"

	     "cur=${COMP_WORDS[COMP_CWORD]}"

	     "res=`clpom --autocomplete $cur`"
	     "COMPREPLY=( $(compgen -W \"$res\" -- $cur) )"
	     "return 0"
	     "}"

	     "complete -F _clpom clpom"))))
