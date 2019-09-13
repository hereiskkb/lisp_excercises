;;; CONTAINS-ARTICLE-P predicate that checks whether article is present in the given set
(defun contains-article-p (x)
	(cond ((not (and (not (member 'the x)) (not (member 'a x)) (not (member 'an x)))) t)))

;;; ADD-VOWELS adds vowels to a given set
(defun add-vowels (x)
	(union '(a e i o u) x))

;;; MY-SUBSETP implementation of SUBSETP using SET-DIFFERENCE
(defun my-subsetp (x y)
	(if (set-difference x y) nil t))

;;; SET-EQUAL to find out if two sets are equal
(defun set-equal (x y)
	(and (subsetp x y) (subsetp y x)))

;;; PROPER-SUBSETP to find out if x is a proper subset of y
(defun proper-subsetp (x y)
	(and (subsetp x y) (not (set-equal x y))))