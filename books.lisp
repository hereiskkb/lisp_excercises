(setf books
	'((harry-potter . j-k-rowling)
		(lord-of-the-rings . j-r-r-tolkein)
		(his-dark-materials . philip-pullman)))

(defun what-wrote (x)
	(car (rassoc x books)))