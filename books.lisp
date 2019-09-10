(setf Books 
	'((harry-potter j-k-rowling)
		(lord-of-the-rings j-r-r-tolkein)
		(the-martian andy-wier)
		(artemis andy-wier)
		(wheel-of-time robert-jordan)))

(defun who-wrote (x)
	(cdr (assoc x Books)))

(defun what-wrote (x)
	())