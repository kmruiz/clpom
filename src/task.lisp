#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

;; task generics
(defgeneric push-step-front (task step))
(defgeneric push-step-back (task step))
(defgeneric run-task (task &optional first-value))
(defgeneric task-depends-on (parent task))

;; step generics
(defgeneric do-step (step value))

(defclass task-step ()
  ((unit-of-work
    :reader unit-of-work
    :initarg :unit-of-work)))

(defun make-task-step (uow) (make-instance 'task-step :unit-of-work uow))

(defmethod do-step ((step task-step) value)
  (apply (unit-of-work step) (list value)))

(defclass task ()
  ((name
    :reader name
    :type string
    :initarg :name)
   (dependencies
    :accessor dependencies
    :reader dependencies
    :type (list task)
    :initarg :dependencies
    :initform (list))
   (steps
    :accessor steps
    :reader steps
    :type (list task-step)
    :initarg :steps
    :initform (list))))

(defun make-task (name &optional &key (dependencies (list)) (steps (list)))
  (make-instance 'task :name name :dependencies dependencies :steps steps))

(defmethod push-step-front ((task task) (step task-step))
  (setf (steps task) (append (list step) (steps task)))
  task)

(defmethod push-step-back ((task task) (step task-step))
  (setf (steps task) (append (steps task) (list step)))
  task)

(defmethod run-task ((task task) &optional first-value)
  (let ((done-list))
    (labels ((do-task-step (task)
	       (reduce (lambda (ret-val step) (do-step step ret-val)) (steps task) :initial-value first-value))
	     (apply-task-deps (task)
	       (dolist (dep-task (dependencies task))
		 (apply-task dep-task)))
	     (apply-task (task)
	       (unless (member task done-list)
		 (apply-task-deps task)
		 (let ((value (do-task-step task)))
		   (push task done-list)
		   value))))
      (apply-task task))))

(defmethod task-depends-on ((parent task) (task task))
  (setf (dependencies parent) (append (dependencies parent) (list task))))
