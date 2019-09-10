(defun fact (x)
	(cond ((zerop x) 1)
			(t (* x (fact (- x 1))))))