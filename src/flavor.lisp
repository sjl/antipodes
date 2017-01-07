(in-package :ap.flavor)

(defparameter *flavor-chance* 1/250)
(defparameter *animals* (read-file-into-form "data/animals.lisp"))

(defun flavorp ()
  (randomp *flavor-chance*))

(defun animal ()
  (format nil "You see ~A ~A.~2%~A"
          (random-elt *animals*)
          (random-elt #("in the distance"
                        "out of the corner of your eye"
                        "running north"))
          (random-elt #("A fellow traveler."
                        "It eyes you warily."
                        "She seems tired."
                        "He seems tired."
                        "A good omen."
                        "Alone, like you."))))

(defun wind ()
  (format nil "A ~A ~A the ~A air against your skin."
          (random-elt #("light breeze"
                        "gentle breeze"
                        "stiff wind"
                        "strong wind"))
          (random-elt #("moves" "pushes"))
          (random-elt #("hot" "warm" "sticky" "humid"))))

(defun random-flavor ()
  (let ((r (random 1.0)))
    (cond ((< r 0.20) (animal))
          (t          (wind)))))