;;; Get last element using last

(defun get-last (x)
	(let ((p (last x)))
		(if (equal (cdr p) nil)
			(car p)
			(cdr p))))

;;; Get last element using REVERSE
(defun get-last-with-reverse (x)
	(car (reverse x)))

;;; Get last element using NTH and LENGTH
(defun get-last-w-nth (x)
	(let ((len (length x)))
		(nth (- len 1) x)))

;;; NEXT-TO-LAST function to return second last element using REVERSE
(defun next-to-last (x)
	(car (cdr (reverse x))))

;;; NEXT-TO-LAST using NTH
(defun next-to-last-w-nth (x)
	(nth 1 (reverse x)))

;;; MY-BUTLAST function to return a list with the last element removed.
(defun my-butlast (x)
	(reverse (cdr (reverse x))))

;;; PALINDROMEP : return T if the list is palindrome
(defun palindromep (x)
	(equal x (reverse x)))

;;; MAKE-PALINDROME : make a palindrome out of a given list
(defun make-palindrome (x)
	(append x (reverse x)))