(setf things
   '((object1 large green shiny cube)
 	 (object2 small red dull metal cube)
 	 (object3 red small dull plastic cube)
 	 (object4 small dull blue metal cube)
 	 (object5 small shiny red four-sided pyramid)
 	 (object6 large shiny green sphere)))

(defun description (x)
	(rest (assoc x things)))

(defun differences (x y)
	(set-exclusive-or (description x)
					  (description y)))

(setf quality-table 
	'((large . size)
		(small . size)
		(red . color)
		(green . color)
		(blue . color)
		(shiny . luster)
		(dull . luster)
		(metal . material)
		(plastic . material)
		(cube . shape)
		(sphere . shape)
		(pyramid . shape)
		(four-sided . shape)))

(defun quality (x)
	(cdr (assoc x quality-table)))

(defun quality-difference(x y)
	(quality (first (differences x y))))

(defun contrast (x y)
	(remove-duplicates
		(sublis quality-table (differences x y))))