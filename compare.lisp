(defun left-side (x)
	(cdr (member '-vs- (reverse x))))

(defun right-side (x)
	(cdr (member '-vs- x)))

(defun count-common (x y)
	(length (intersection x y)))

(defun compare (x)
	(list (count-common (left-side x) (right-side x)) 'Common 'features))