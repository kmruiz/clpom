(lambda (project)
  (add-task project "say-pi" "fun"
	    (lambda (-)
	      (log-info "3.1416"))))
