(defun laugh (x)
	"Returns a list containing 'HA' the number of times specified by x"
	(cond ((equal x 0) nil)
			(t (cons 'ha (laugh (- x 1))))))

(defun add-up (x)
	"Add the numbers in the list x"
	(cond ((null x) 0)
			(t (+ (car x) (add-up (rest x))))))

(defun alloddp (x)
	"Returns t if all the numbers are odd"
	(cond 	((null  (car x)) nil)
			((evenp (car x)) t)
			(t (alloddp (rest x)))))

(defun rec-member (x y)
	"Member function implemented with recursion"
	(cond ((null x) nil)
			((equal y (car x)) x)
			(t (rec-member (rest x) y))))

(defun rec-assoc (x y)
	"Assoc function implemented with recursion"
	(cond ((null y) nil)
			((equal x (car (first y))) (car y))
			(t (rec-assoc x (rest y)))))

(defun rec-nth (x y)
	"Recursive version of nth"
	(cond ((null y) nil)
			((equal x 0) (car y))
			(t (rec-nth (- 1 x) (rest y)))))

(defun add1 (x)
	(+ x 1))

(defun sub1 (x)
	(- x 1))

(defun rec-plus (x y)
	"Recursive implementation of +"
	(cond ((zerop y) x)
			(t (rec-plus (add1 x) (sub1 y)))))

(defun fib (x)
	"Returns the fibonacci number at the specified position"
	(cond ((equal x 1) 1)
			((equal x 0) 1)
			(t (+ (fib (- x 1)) (fib (- x 2))))))

(defun any-7-p (x)
	"Searches the list for 7"
	(cond ((equal (first x) 7) t)
			(t (any-7-p (rest x)))))
