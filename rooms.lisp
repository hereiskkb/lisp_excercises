(setf rooms
	'((library					(east 	upstairs-bedroom)
								(south 	back-stairs))
	  (upstairs-bedroom			(west	library)
	  							(south	front-stairs))
	  (back-stairs				(north	library)
	  							(south	downstairs-bedroom))
	  (front-stairs				(north	upstairs-bedroom)
	  							(south	living-room))
	  (downstairs-bedroom		(north	back-stairs)
	  							(east	dining-room))
	  (living-room				(north	front-stairs)
	  							(south	dining-room)
	  							(east	kitchen))
	  (dining-room				(west	downstairs-bedroom)
	  							(north	living-room)
	  							(east	pantry))
	  (kitchen					(west	living-room)
	  							(south	pantry))
	  (pantry					(north	kitchen)
	  							(west	dining-room))))

;;; Global Variable LOC to hold player's current position
(setf LOC 'pantry)

(defun choices (x)
	"Takes the name of the room as input and returns a 
	table of permissible directions to take"
	(cdr (assoc x rooms)))

(defun look (x y)
	"Takes two inputs, a direction and a room, and tells
	where you would end up if you move in that direction
	from that room"
	(car (cdr (assoc x (choices y)))))

(defun set-player-location (place)
	"Set the current location of player"
	(setf LOC place))

(defun how-many-choices ()
	"How many choices does the player have depending on
	where they are now"
	(length (choices LOC)))

(defun upstairsp (x)
	"Predicate to indicate upstairs location"
	(cond ((or (equal x 'upstairs-bedroom) (equal x 'library)) t)
			(T nil)))

(defun onstairsp (x)
	"Predicate to indicate the stairs"
	(cond ((or (equal x 'front-stairs) (equal x 'back-stairs)) t)
			(T nil)))

(defun where ()
	"Says where you are"
	(cond ((upstairsp LOC) (list 'you 'are 'upstairs 'in 'the LOC))
		((onstairsp LOC) (list 'You 'are 'on 'the LOC))
		(T (list 'you 'are 'downstairs 'in 'the LOC))))

(defun move (direction)
	"Takes the direction as input and moves player in that
	direction"
	(cond ((look direction LOC) (set-player-location (look direction LOC))
								(list 'you 'are 'now 'in LOC))
		(T (list 'ouch! 'hit 'a 'wall))))