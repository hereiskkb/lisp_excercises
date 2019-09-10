(defun rotate-left (x)
	"Rotates the given list right by one element"
	(reverse (cons (car x) (reverse (cdr x)))))

(defun rotate-right (x)
	"Rotates the given list left by one element"
	(cons (car (reverse x)) (reverse (cdr (reverse x)))))