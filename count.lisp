(defun count-up (n)
	(count-up-tr n nil))

(defun count-up-tr (n x)
	(cond ((zerop n) x)
			(t (count-up-tr (- n 1) (cons n x)))))

(defun fact (n)
	(fact-tr n 1))

(defun fact-tr (n x)
	(cond ((equal n 1) x)
			(t (fact-tr (- n 1) (* x n)))))