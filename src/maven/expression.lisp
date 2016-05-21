#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defclass maven-context ()
  ((parent :accessor parent :reader parent :initform nil)
   (properties :accessor properties :reader properties :initform (make-hash-table :test 'equal))))

(defgeneric add-property (ctx property value))
(defgeneric parse-expression (ctx expression))

(defmethod add-property ((ctx maven-context) (property string) (value string))
  (setf (gethash property (properties ctx)) value))

(defmethod parse-expression ((ctx maven-context) value)
  (cond
    ((null value) "")
    (t value)))

(defmethod parse-expression ((ctx maven-context) (value string))
  (cond
    ((< (length value) 4) value)
    ((string= value "${" :end1 2)
     (let ((hash (gethash (subseq value 2 (- (length value) 1)) (properties ctx))))
       (if (null hash) (parse-expression (parent ctx) value) hash)))
    (t value)))
