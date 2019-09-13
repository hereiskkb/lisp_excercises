(setf database 
	'(  (b1 shape brick)
		(b1 color green)
		(b1 size small)
		(b1 supported-by b2)
		(b1 supported-by b3)
		(b2 shape brick)
		(b2 color red)
		(b2 size small)
		(b2 left-of b3)
		(b2 supports b1)
		(b3 shape brick)
		(b3 color red)
		(b3 size small)
		(b3 right-of b2)
		(b3 supports b1)
		(b4 shape pyramid)
		(b4 color blue)
		(b4 supported-by b5)
		(b4 size large)
		(b5 shape cube)
		(b5 color green)
		(b5 supports b4)
		(b6 color purple)
		(b6 size large)
		(b6 shape brick)))

(defun match-element (elem1 elem2)
	"Returns T if elem1 and elem2 are equal or if elem2 is a question mark."
	(cond ((equal elem1 elem2) T)
		((equal elem2 '?) T)
		(T nil)))

(defun match-triple (assertion pattern)
	"Return true if the assertion matches the pattern"
	(every #'match-element assertion pattern))

(defun fetch (pattern)
	"Takes a pattern as input and returns all the matching entries in the database"
	(remove-if-not #'(lambda (entry) (if (match-triple entry pattern) entry)) database))

(defun block-color-? (b)
	"Takes a block name and returns the pattern to get the color of the block"
	(list b 'color '?))

(defun supported-by-? (b)
	"Takes a block name and returns the pattern to get the supported-by docs"
	(list b 'supported-by '?))

(defun supporters (b)
	"Takes a block name as input and returns the name of blocks that support it"
	(mapcar #'third (fetch (supported-by-? b))))

(defun supp-cube (b)
	"Takes a block as an input and returns true if the block is supported-by a cube"
	(if (find-if #'(lambda (entry) (if (equal (third (first(fetch (list entry 'shape '?)))) 'cube) T nil)) (supporters b)) t nil))

(defun desc1 (b)
	"Returns all the matching assertions about the given block"
	(remove-if-not #'(lambda (entry) (equal (first entry) b)) database))

(defun description (b)
	"Returns the description of a block"
	(reduce #'append (mapcar #'(lambda (entry) (list (second entry) (third entry))) (desc1 b))))