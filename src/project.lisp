#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package #:clpom)

;; project generics

(defgeneric get-extra (project key))
(defgeneric put-extra (project key value))
(defgeneric add-task  (project task category unit-of-work))
(defgeneric add-task-dependency (project task dependency))
(defgeneric run-project-task (project task))
(defgeneric register-plugin (project plugin))

(defclass project ()
  ((name
    :reader name
    :type string
    :initarg :name)
   (extras
    :accessor extras
    :reader extras
    :type list
    :initarg :extras
    :initform (list))
   (tasks
    :accessor tasks
    :reader tasks
    :type list
    :initarg :tasks
    :initform (list))))

(defun make-project (name)
  (make-instance 'project :name name))

(defmethod get-extra ((project project) (key symbol))
  (getf (extras project) key))

(defmethod put-extra ((project project) (key symbol) value)
  (setf (getf (extras project) key) value))

(defmethod add-task ((project project) (task string) (category string) (unit-of-work function))
  (flet ((task-name-matches (task-ref)
	   (string= (name task-ref) task)))
    (let ((task-ref (find-if #'task-name-matches (tasks project))))
      (cond
	((null task-ref)
	 (setq task-ref (make-task task :category category))
	 (push task-ref (tasks project))
	 (push-step-back task-ref (make-task-step unit-of-work)))
	(t
	 (setf (category task-ref) category)
	 (push-step-back task-ref (make-task-step unit-of-work)))))))

(defmethod add-task-dependency ((project project) (task string) (dependency string))
  (flet ((task-name-matches (name)
	   (lambda (task-ref) (string= name (name task-ref)))))
    (let ((task-ref (find-if (task-name-matches task) (tasks project)))
	  (dep-ref (find-if (task-name-matches dependency) (tasks project))))
      (when (null task-ref)
	(setq task-ref (make-task task))
	(push task-ref (tasks project)))
      (when (null dep-ref)
	(setq dep-ref (make-task dependency))
	(push dep-ref (tasks project)))
      (task-depends-on task-ref dep-ref))))

(defmethod run-project-task ((project project) (task string))
  (flet ((task-name-matches (task-ref)
	   (string= (name task-ref) task)))
    (let ((task-ref (find-if #'task-name-matches (tasks project))))
      (cond
	((null task-ref)
	 (log-error "Task ~a does not exist on project" task)
	 (log-info "Do '~a' for a task list" "clpom help"))
	(t (run-task task-ref))))))

(defmethod register-plugin ((project project) (plugin function))
  (apply plugin (list project)))
