(defvar family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

(defun father (x)
    "Return the father of the child."
    (second (assoc x family)))

(defun mother (x)
    "Return the mother of the child."
    (third (assoc x family)))

(defun parents (x)
    "Return both parents of the child."
    (list (father x) (mother x)))

(defun children (x)
    "Return the children of the provided node."
    (remove-if #'null (mapcar #'(lambda (entry) (if (or (equal x (second entry)) (equal x (third entry))) (first entry))) family)))

(defun siblings (x)
    "Return siblings of the given node"
    (remove-if #'(lambda (e) (equal e x)) (reduce #'union (remove-if #'null (mapcar #'children (parents x))))))

(defun mapunion (func x)
    "Apply given function to the list and union to the result"
    (reduce #'union (mapcar func x)))

(defun grandparents (x)
    "Find set representing the grandparent of x"
    )