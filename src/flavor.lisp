(in-package :ap.flavor)

(defparameter *animals* (read-file-into-form "data/animals.lisp"))
(defparameter *cooldown* 0)

(defun flavorp ()
  (if (plusp *cooldown*)
    (progn (decf *cooldown*) nil)
    (if (randomp ap::*flavor-chance*)
      (progn (setf *cooldown* ap::*flavor-cooldown*)
             t)
      nil)))

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

(defun feeling ()
  (format nil "You suddenly have ~A feeling.~2%~A"
          (random-elt #("a good" "a bad" "a worrying" "a hopeful" "an uneasy"))
          (random-elt #("It passes after a moment."
                        "It lingers for a while."))))

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
          ((< r 0.80) (feeling))
          (t          (wind)))))
