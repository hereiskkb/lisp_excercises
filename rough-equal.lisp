(defun rough-equal (key paralist)
	(find-if #'(lambda (entry) (cond ((and (> entry (- key 10)) (< entry (+ key 10))) entry)
					(t nil))) paralist))

(defun find-nested (x)
	(find-if #'(lambda (entry) (equal (type-of entry) 'cons)) x))