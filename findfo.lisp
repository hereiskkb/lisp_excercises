(defun find-first-odd (x)
	"Find the first odd number"
	(cond ((null x) nil)
			((oddp (first x)) (first x))
			(t (find-first-odd (rest x)))))

(defun find-first-atom (x)
	"Find the first atom in a list"
	(cond ((atom x) x)
			(t (find-first-atom (first x)))))

(defun last-element (x)
	"Find the last element of the given list"
	(cond ((atom (cdr x)) (car x))
			(t (last-element (rest x)))))

(defun anyoddp-mod (x)
	"anyoddp implemented by eliminating the null check"
	(cond ((oddp (first x)) t)
			(t (anyoddp-mod (rest x)))))

(defun add-nums (x)
	"Add all numbers leading to the given number"
	(cond ((equalp 0 x) 0)
			(t (+ x (add-nums (- x 1))))))

(defun all-equal (x)
	"Returns true if all the elements of the list are equal"
	(cond ((< (length x) 2) t)
			(t (if (not (equal (first x) (second x))) nil 
													(all-equal (rest x))))))