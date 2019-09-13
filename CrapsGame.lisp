(defun throw-die ()
	(+ 1 (random 6)))

(defun throw-dice ()
	(list (throw-die) (throw-die)))

(defun snake-eyes-p (x)
	(if (and (equal (first x) (second x))) t))

(defun boxcars-p (x)
	(if (and (equal (first x) (second x))) t))

(defun instant-win-p (x)
	(let ((sumresult (+ (first x) (second x))))
		(cond ((or (equal sumresult 7) (equal sumresult 11)) t)
		(t nil))))

(defun instant-loss-p (x)
	(let ((sumresult (+ (first x) (second x))))
		(cond ((or (equal sumresult 2) (equal sumresult 3) (equal sumresult 12)) t)
		(t nil))))

(defun say-throw (x)
	(cond ((snake-eyes-p x) 'SNAKE-EYES)
		  ((boxcars-p x) 'BOXCARS)
		  (t (+ (first x) (second x)))))

(defun craps ()
	(let* ((throw (throw-dice))
			(resType (say-throw throw)))
			(cond ((instant-win-p throw) (list 'Throw (first throw) 'and (second throw) '-- resType '-- 'You 'Win))
					((instant-loss-p throw) (list 'Throw (first throw) 'and (second throw) '-- resType '-- 'You 'Lose))
					(t (list 'Throw (first throw) 'and (second throw) '-- 'Your 'Score 'is (+ (first throw) (second throw)))))))

(defun try-for-point (x)
	(let* ((throw (throw-dice))
			(sum (+ (first throw) (second throw))))
		(cond ((equal sum x) (list 'Throw (first throw) 'and (second throw) '- sum '- 'You 'win))
				((equal sum 7) (list 'Throw (first throw) 'and (second throw) '- sum '- 'You 'Lose))
				(t (list 'Throw (first throw) 'and (second throw) '- sum 'Throw 'Again)))))