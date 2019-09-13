(defun rotate-left (x)
	"Rotates the elements of the list to the left"
	(let ((cdrlast (cdr (last x)))
		  (cdrfirst (cdr x)))
		(setf cdrlast (car x))
		(setf cdrfirst nil)))

(defun rotate-right (x)
	"Rotates the elements of the list to the right"
	)