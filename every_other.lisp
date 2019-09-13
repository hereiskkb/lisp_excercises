(defun every-other (x)
	(cond ((null x) nil)
			(t (cons (car x) (every-other (member (car (cdr (cdr x))) x))))))

(defun left-half (x)
	(let ((xlength (length x)))
			(if (equal xlength 0) nil (get-left x (/ xlength 2.0)))))

(defun get-left (x y)
	(cond ((not (> y 0)) nil)
			(t (cons (car x) (get-left (rest x) (- y 1))))))

(defun merge-lists (x y)
	(sort (append x y) #'<))

(defun merge-lists (x y)
	(cond ((null x) y)
			((null y) x)
			((< (first x) (first y)) (cons (first x) (merge-lists (rest x) y)))
			(t (cons (first y) (merge-lists x (rest y))))))