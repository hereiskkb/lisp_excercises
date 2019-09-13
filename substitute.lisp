(defun rin-find-if (y x)
	"Find-if using "
	(first (remove-if-not #'(lambda (e) (if (not (#'y e)) e)) x)))