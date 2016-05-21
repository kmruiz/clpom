#|
This file is part of clpom project object model
(c) 2016 Kevin Mas <masruizkevin@gmail.com>
See LICENSE for more information
|#

(in-package :clpom)

(defun %find-node (list name)
  (find-if
   (lambda (x)
     (cond
       ((consp (first x)) (string= (first (first x)) name))
       ((stringp (first x)) (string= (first x) name))
       (t nil))) list))

(defun %to-string (response)
  (if
   (stringp response) response
   (flexi-streams:octets-to-string response)))

(defun metadata-for-package-selector (maven-dependency-selector accepted-repositories)
  (mapcar
   (lambda (repository)
     (let ((url (metadata-file-for-repository maven-dependency-selector repository)))
       (log-trace "Looking for package ~a in ~a" (name maven-dependency-selector) url)
       (multiple-value-bind (content http-result response-headers final-url)
	   (drakma:http-request url)
	 (case http-result
	   (200
	    (let* ((xml-content (%to-string content))
		   (xml-cons (xmls:parse xml-content)))
	      (return-from metadata-for-package-selector
		(let* ((versioning
			(find-if
			 (lambda (x)
			   (string= (first x) "versioning")) (cdddr xml-cons)))
		       (all-versions (cddr versioning)))
		  (let ((latest
			 (third (%find-node all-versions "latest")))
			(release
			 (third (%find-node all-versions "release")))
			(versions
			 (cddr (%find-node all-versions "versions"))))
		    (values
		     repository
		     (maven-artifact-for-version maven-dependency-selector latest)
		     (maven-artifact-for-version maven-dependency-selector release)
		     (mapcar (lambda (version)
			       (maven-artifact-for-version maven-dependency-selector (third version)))
			     versions)))))))
	   (t (log-trace "Http-Status ~a. Could not find ~a in repository ~a"
			 http-result (name maven-dependency-selector) repository))))))
   accepted-repositories))

(defun download-to-local-repository (artifact from-url local-repo extension)
  (let ((file (url-for-maven-artifact artifact local-repo extension)))
    (ensure-directories-exist file)
    (with-open-file (stream file
			    :direction :output
			    :element-type '(unsigned-byte 8)
			    :if-does-not-exist :create
			    :if-exists :overwrite)
      (let ((response (drakma:http-request from-url)))
	(write-sequence response stream)
	(log-info "~a length" (length response))))
    t))

(defun compute-dependencies (artifact repository)
  (let* ((pom (%to-string (drakma:http-request (url-for-maven-artifact artifact repository ".pom"))))
	 (xml (xmls:parse pom))
	 (dependencies-nodes (cddr (%find-node (cddr xml) "dependencies")))
	 (property-nodes (cddr (%find-node (cddr xml) "properties")))
	 (ctx (make-instance 'maven-context)))
    (format t "~%************~%~a~%***********~%" pom)
    (loop for i in property-nodes
       do (format t "found property ~a~%" i)
       do (add-property ctx (first (first i)) (or (parse-expression ctx (or (third i) "null")) "")))

    (add-property ctx "project.version" (slot-value artifact 'version))
    (add-property ctx "project.groupId" (slot-value artifact 'group))
    (add-property ctx "project.artifactId" (slot-value artifact 'artifact))
    
    (alexandria:flatten
     (mapcar
      (lambda (dependency)
	(let ((found-artifact
	       (make-maven-artifact
		:group (third (%find-node dependency "groupId"))
		:artifact (third (%find-node dependency "artifactId"))
		:version (parse-expression ctx (third (%find-node dependency "version")))
		:scope (third (%find-node dependency "scope")))))
	  (if (string= (slot-value found-artifact 'scope) "test") nil
	      (progn
		(log-info "Found artifact: ~a" found-artifact)
		(concatenate 'list (list found-artifact)
			     (compute-dependencies found-artifact repository))))))
      dependencies-nodes))))

(defun download-artifact (maven-dependency-selector local-repository accepted-repositories)
  (multiple-value-bind (repository latest release versions)
      (metadata-for-package-selector maven-dependency-selector accepted-repositories)
    (let* ((selected (select-maven-artifact maven-dependency-selector latest release versions)))
      (return-from download-artifact (compute-dependencies selected repository))
      (cond
	((null selected)
	 (log-error "No candidate for ~a in ~a" (name maven-dependency-selector) versions))
	(t
	 (download-to-local-repository
	  selected
	  (url-for-maven-artifact selected repository ".jar")
	  local-repository
	  ".jar"))))))
