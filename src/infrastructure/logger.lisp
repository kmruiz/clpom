#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defconstant +default-color+ cl-ansi-text:+reset-color-string+)
(defparameter ref/log-levels ())

(defmacro with-output-supression (&body body)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     (with-open-stream (*error-output* (make-broadcast-stream))
       ,@body)))

(defun %wrap-on-color (color text)
  (format nil "~a~a~a"
	  (cl-ansi-text:make-color-string color)
	  text
	  +default-color+))

(defun %log-level (string)
  (getf ref/log-levels (intern (string-upcase string))))

(defun %should-print (status &optional &key (min-log (env/string "CLPOM_LOG")))
  (let ((min-log (or (%log-level min-log)
		     (%log-level "INFO")))
	(status-log (%log-level status)))
    (>= status-log min-log)))

(defun log-msg (status status-color message)
  (when (%should-print status)
    (let ((final-fmt (format nil "~&[~10a] ~a~&" (%wrap-on-color status-color status) (car message))))
      (let ((fmt (append '(format t) (list final-fmt) (rest message))))
	(eval fmt)))))

(defmacro define-log-level (log-level status-color &key level)
  `(progn
     (setf (getf ref/log-levels ',(intern log-level)) ,level)
     (defun ,(intern (string-upcase (format nil "log-~a" log-level))) (message &rest fmt-values)
       (log-msg ,(string-upcase log-level) ,status-color (append (list message) fmt-values)))))

(define-log-level "TRACE" :cyan :level 1)
(define-log-level "INFO" :blue :level 2)
(define-log-level "WARN" :yellow :level 3)
(define-log-level "ERROR" :red :level 4)
(define-log-level "DONE" :green :level 5)
