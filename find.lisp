(defun find-number (x)
	(cond ((numberp x) x)
				((atom x) nil)
				(t (or (find-number (car x))
								(find-number (cdr x))))))

(defun atoms-to-q (x)
	(cond ((null x) nil)
			((atom x) 'q)
			(t (cons (atoms-to-q (car x))
									(atoms-to-q (cdr x))))))

(defun count-atoms (x)
	"Counts the number of atoms in the list"
	(cond 	((atom x) 1)
			(t (+ (count-atoms (car x)) (count-atoms (cdr x))))))

(defun count-cons (x)
	"Count the number of cons cells"
	(cond ((null x) 0)
			((consp x) (+ 1 (count-cons (car x)) (count-cons (cdr x))))
			(t 0)))

(defun sum-tree (x)
	"Recursively calculate the sum of all the numbers appearing in the tree"
	(cond ((or (null x) (symbolp x)) 0)
			((numberp x) x)
			(t (+ (sum-tree (car x)) (sum-tree (cdr x))))))

(defun my-subst (x y z)
	"Recursive implementation of subst"
	(cond ((null z) nil)
			((atom z) (if (equal y z) x z))
			(t (cons (my-subst x y (car z)) (my-subst x y (cdr z))))))

(defun flatten (x)
	"Flattens a given nested list to the same level"
	(cond ((null x) nil)
			((atom x) (append (cons x nil) nil))
			(t (append (flatten (car x)) (flatten (cdr x))))))

(defun tree-depth (x)
	"Finds the maximum depth of a given tree"
	(cond ((null x) 0)
			((atom x) 0)
			(t (if (> (+ 1 (tree-depth (car x))) (+ 1 (tree-depth (cdr x)))) (+ 1 (tree-depth (car x))) (+ 1 (tree-depth (cdr x)))))))

(defun paren-depth (x)
	"Finds the maximum depth of the nested parentheses in a list"
	(cond ((null x) 0)
			((atom x) 0)
			(t (if (> (+ 1 (paren-depth (car x))) (+ 0 (paren-depth (cdr x)))) (+ 1 (paren-depth (car x))) (+ 0 (paren-depth (cdr x)))))))

