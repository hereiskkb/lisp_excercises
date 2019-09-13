(defun titledp (name)
	(member (first name) '(mr ms miss mrs)))

(setf male-names '(john kim richard fred george))

(setf female-names '(jane mary wanda barbara kim))

(defun malep (name)
	(and (member name male-names) (not (member name female-names))))

(defun femalep (name)
	(and (member name female-names) (not (member name male-names))))

(defun give-title (name)
	"Returns a name with an appropriate title on the front"
	(cond ((titledp name) name)
		((malep (first name)) (cons 'mr name))
		((femalep (first name)) (cons 'ms name))
		(t (append '(mr or ms) name))))