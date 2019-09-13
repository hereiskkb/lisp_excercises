(setf my-hand 
	'((3 hearts)
		(5 clubs)
		(2 diamonds)
		(4 diamonds)
		(ace spades)))

(setf colors 
	'((clubs black)
		(diamonds red)
		(hearts red)
		(spades black)))

(setf all-ranks '(2 3 4 5 6 7 8 9 jack queen king ace))

(defun rank (card)
	"Returns the rank of the card"
	(car card))

(defun suit (card)
	"Returns the suit of the card"
	(second card))

(defun count-suit (suit hand)
	"Returs the number of cards of the suit in the hand"
	(length (remove-if-not #'(lambda (e) (equal (suit e) suit)) hand)))

(defun color-of (card)
	"Returns the color of the card passed"
	(second (assoc (suit card) colors)))

(defun first-red (hand)
	"Returns the first card of a hand that has red suit"
	(first (remove-if-not #'(lambda (e) (equal (color-of e) 'red)) hand)))

(defun black-cards (hand)
	"Returns all the cards that have black in the hand"
	(remove-if-not #'(lambda (e) (equal (color-of e) 'black)) hand))

(defun get-cards (incoming-suit hand)
	"Get all the cards of a given suit"
	(remove-if-not #'(lambda (e) (equal (suit e) incoming-suit)) hand))

(defun what-ranks (incoming-suit hand)
	"Returns the rank of a given suit among all the cards in a hand"
	(mapcar #'(lambda (e2) (first e2)) (get-cards incoming-suit hand)))

(defun higher-rank-p (card1 card2)
	"Takes two cards and returns T if the first card has higher rank than second"
	(< (length (member (rank card1) all-ranks)) (length (member (rank card2) all-ranks))))

(defun high-card (hand)
	"Returns the highest ranked card in the hand"
	 (find-if #'(lambda (e) (assoc e hand)) (reverse all-ranks)))