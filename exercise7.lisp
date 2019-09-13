(defun pick-out (x)
	"Return the elements that are greater than one and less than 5 in the list"
	(remove-if-not #'(lambda (e) (and (> e 1) (< e 5))) x))

(defun count-the (x)
	"Count the number of 'the' in a given sentence"
	(length (remove-if-not #'(lambda (e) (equal 'the e)) x)))

(defun pick-twos (x)
	"pick those lists that have a length of exactly 2"
	(remove-if-not #'(lambda (e) (equal (length e) 2)) x))

(defun intersection-mine (x y)
	"find intersection between two sets"
	(remove-if-not #'(lambda (e) (member e y)) x))