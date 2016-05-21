#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun string-replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 



(defstruct maven-artifact
  group artifact classifier version scope optional)

(defclass maven-dependency-selector ()
  ((group :initarg :group :type 'string :accessor group :reader group)
   (artifact :initarg :artifact :type 'string :accessor artifact :reader artifact)
   (classifier :initform nil :initarg :classifier :type 'string :accessor classifier :reader classifier)
   (version :initform :release :initarg :version :accessor version :reader version)))

(defgeneric select-maven-artifact (selector latest release other-artifacts))
(defgeneric metadata-file-for-repository (selector repository))
(defgeneric name (selector))
(defgeneric maven-artifact-for-version (selector version))

(defmethod select-maven-artifact ((selector maven-dependency-selector) (latest maven-artifact) (release maven-artifact) other-artifacts)
  (let ((version (version selector)))
    (cond
      ((eql version :latest) latest)
      ((eql version :release) release)
      ((stringp version)
       (find-if (lambda (x) (string= (version x) version))
		(concatenate 'list (list latest release) other-artifacts))))))

(defmethod metadata-file-for-repository ((selector maven-dependency-selector) (repository string))
  (format nil "~a/~a/maven-metadata.xml"
	  repository
	  (string-replace-all
	   (concatenate 'string
			(group selector)
			"."
			(artifact selector)) "." "/")))

(defmethod name ((selector maven-dependency-selector))
  (format nil "~a:~a:~a:~a" (group selector) (artifact selector) (classifier selector) (version selector)))

(defmethod maven-artifact-for-version ((selector maven-dependency-selector) (version string))
  (make-maven-artifact
   :group (group selector)
   :artifact (artifact selector)
   :classifier (classifier selector)
   :version version))

(defun url-for-maven-artifact (artifact repository extension)
  (with-slots (group artifact classifier version) artifact
    (cond
      ((null classifier)
       (format nil "~a/~a/~a/~a/~a-~a~a"
	       repository
	       (string-replace-all group "." "/")
	       (string-replace-all artifact "." "/")
	       version
	       artifact
	       version
	       extension))
      (t
       (format nil "~a/~a/~a/~a/~a-~a-~a~a"
	       repository
	       (string-replace-all group "." "/")
	       (string-replace-all artifact "." "/")
	       version
	       artifact
	       classifier
	       version
	       extension)))))
