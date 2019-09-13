(setf *print-circle* t)

(setf nerd-states '(sleeping eating waiting-for-a-computer programming debugging))

(defun circular! ()
	"Create a circular list given a list"
	(setf (cdr (last nerd-states)) nerd-states))

(circular!)

(defun nerdus (state)
	"Gives the next state given a state from the nerd states"
	(car (cdr (member state nerd-states))))

(defun sleepless-nerd (state)
	"Same as NERDUS but skips sleep"
	(let ((next-state (nerdus state)))
		(if (equal next-state 'sleeping)
			(nerdus 'sleeping) 
			next-state)))

(defun nerd-on-caffeine (state)
	"Skips two steps"
	(nerdus (nerdus state)))