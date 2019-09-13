(defun swap-first-last (x)
	"Swaps first and last elements of a list"
	(let ((first-word (first x))
			(last-word (car (last x))))
		(setf (car x) last-word)
		(setf (car (last x)) first-word)))