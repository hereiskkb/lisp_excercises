(defun count-down (n)
	"Counts down from n using list-consing recursion"
	(cond ((equal 0 n) nil)
			(t (cons n (count-down (- n 1))))))

(defun count-fact (n)
	"An applicative version of fact using count-down"
	(reduce #'* (count-down n)))

(defun square-list (x)
	"Returns squared list"
	(cond ((null x) nil)
			(t (cons (* (first x) (first x)) (square-list (rest x))))))

(defun my-nth (n x)
	(cond ((zerop n) (first x))
			((null x) nil)
			(t (my-nth (- n 1) (rest x)))))

(defun my-member (x y)
	"recursive member"
	(cond ((null y) nil)
			((equal x (first y)) y)
			(t (my-member x (rest y)))))

(defun my-assoc (x y)
	"recursive assoc"
	(cond ((null y) nil)
			((equal x (first (first y)))  (first y))
			(t (my-assoc x (rest y)))))

(defun compare-length (x y)
	"Find length of each other recursively"
	(cond ((and (null x) (null y)) 'SAME-LENGTH)
			((and (null x) (not (null y))) 'SECOND-IS-LONGER)
			((and (not (null x)) (null y)) 'FIRST-IS-LONGER)
			(t (compare-length (rest x) (rest y)))))

(defun extract-symbols (x)
	"Extract symbols from a list"
	(cond ((null x) nil)
			((symbolp (first x))
				(cons (first x)
					(extract-symbols (rest x))))
			(t (extract-symbols (rest x)))))

(defun sum-numeric-elements (x)
	"Add all the numbers in the list and ignore the non-numbers"
	(cond ((null x) 0)
			((numberp (first x)) (+ (first x) (sum-numeric-elements (rest x))))
			(t (sum-numeric-elements (rest x)))))

(defun my-remove (x y)
	"Recursive version of remove function"
	(cond ((null y) nil)
			((equal x (first y)) (my-remove x (rest y)))
			(t (cons (first y) (my-remove x (rest y))))))

(defun my-intersection (x y)
	"recursive intersection function"
	(cond ((null x) nil)
			((not (null (member (first x) y))) (cons (first x) (my-intersection (rest x) y)))
			(t (my-intersection (rest x) y))))

(defun my-set-difference (x y)
	"recursive set difference"
	(cond ((null x) nil)
			((null (member (first x) y)) (cons (first x) (my-set-difference (rest x) y)))
			(t (my-set-difference (rest x) y))))

(defun count-odd (x)
	"count the number of odd numbers in the list recursively"
	(cond ((null x) 0)
			((oddp (first x)) (+ 1 (count-odd (rest x))))
			(t (count-odd (rest x)))))

