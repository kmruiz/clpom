#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defconstant +default-color+ cl-ansi-text:+reset-color-string+)

(defun %wrap-on-color (color text)
  (format nil "~a~a~a"
	  (cl-ansi-text:make-color-string color)
	  text
	  +default-color+))

(defun log-msg (status status-color message)
  (let ((final-fmt (format nil "~&[~10a] ~a~&" (%wrap-on-color status-color status) (car message))))
    (let ((fmt (append '(format t) (list final-fmt) (rest message))))
      (eval fmt))))

(defmacro define-log-level (log-level status-color)
  `(defun ,(intern (string-upcase (format nil "log-~a" log-level))) (message &rest fmt-values)
     (log-msg ,(string-upcase log-level) ,status-color (append (list message) fmt-values))))

(define-log-level "TRACE" :cyan)
(define-log-level "INFO" :blue)
(define-log-level "WARN" :yellow)
(define-log-level "ERROR" :red)
(define-log-level "DONE" :green)
