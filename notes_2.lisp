(setf note-table
	' ( (C 			1)
		(C-sharp 	2)
		(D 			3)
		(D-sharp 	4)
		(E 			5)
		(F 			6)
		(F-sharp 	7)
		(G 			8)
		(G-sharp 	9)
		(A 			10)
		(A-sharp 	11)
		(B 			12)))

(defun numbers (note-list)
	"Return list of corresponding numbers for a list of notes"
	(mapcar #'(lambda (entry) (second (assoc entry note-table))) note-list))

(defun search-by-value (value table)
	"search in a table by table and return the entry of the table"
	(find-if #'(lambda (entry) (if (equal value (second entry)) entry)) table))

(defun notes (note-numbers)
	"Makes a list of numbers as input"
	(mapcar #'(lambda (entry) (first (search-by-value entry note-table))) note-numbers))

(defun raise (n note-numbers)
	"Raises a note by a given number"
	(mapcar #'(lambda (entry) (+ n entry)) note-numbers))

(defun normalize (note-numbers)
	"normalize out of range numbers"
	(mapcar #'(lambda (entry) (cond ((> entry 12) (- entry 12))
										((< entry 1) (+ entry 12))
										(t entry))) note-numbers))

(defun transpose (n song)
	"Returs the song transposed by n half-steps"
	(notes (normalize (raise n (numbers song)))))