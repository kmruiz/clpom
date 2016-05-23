(lambda (project)
  (add-task project "say-pi"
	    (lambda (-)
	      (log-info "3.1416"))))
