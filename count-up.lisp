(defun count-up (x y)
	"Counts up from x to y"
	(cond ((> x y) nil)
			(t (cons x (count-up (+ 1 x) y)))))

(defun make-loaf (x)
	"Prints a loaf of bread using x"
	(print-loaf x))

(defun print-loaf (x)
	(if (equal x 0) nil (cons 'x (print-loaf (- x 1)))))

(defun bury (x n)
	"Bury the given symbol under n levels of parentheses"
	(print-bury x n))

(defun print-bury (x n)
	(cond ((equal n 0) x)
			(t (cons (print-bury x (- n 1)) nil))))

(defun pairings (x y)
	"Function that pairs the elements of two lists"
	(pair x y))

(defun pair (x y)
	(cond ((or (null x) (null y)) nil)
			(t (cons (list (first x) (first y)) (pair (cdr x) (cdr y))))))

(defun sublists (x)
	"Function that creates sub lists out of a given list"
	(division x))

(defun division (x)
	(cond ((null x) nil)
			(t (cons x (division (cdr x))))))

(defun my-reverse (x)
	"Recursive version of reverse"
	(reverse-c x (length x)))

(defun reverse-c (x y)
	(cond ((equal y 0) nil)
			(t (append (reverse-c (rest x) (- y 1)) (cons (first x) nil)))))

(defun my-union (x y)
	"Recursive union function"
	(union-c x y))

(defun union-c (x y)
	(cond ((null x) y)
			(t (if (member (car x) y) (union-c (rest x) y) (cons (car x) (union-c (rest x) y))))))

(defun largest-even (x)
	"returns the largest even number in a given list"
	(apply #'max (find-evens x)))

(defun find-evens (x)
	(cond ((null x) (cons 0 nil))
			((evenp (car x)) (append (find-evens (rest x)) (cons (car x) nil)))
			(t (find-evens (rest x)))))

(defun huge (x)
	"Calculate the power of number over itself Recursively"
	(power x x))

(defun power (x y)
	(cond ((equal y 0) 1)
			(t (* x (power x (- y 1))))))