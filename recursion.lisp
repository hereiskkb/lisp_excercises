(defun my-reverse (x)
	(cond ((null x) nil)
			(t (append (reverse (rest x))
						(list (first x))))))