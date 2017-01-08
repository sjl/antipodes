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
                        "heading north"))
          (random-elt #("A fellow traveler."
                        "It eyes you warily."
                        "She seems tired."
                        "He seems tired."
                        "A good omen."
                        "Alone, like you."))))

(defun feeling ()
  (format nil "You suddenly have ~A feeling.~2%~A"
          (random-elt #("a good"
                        "a bad"
                        "a worrying"
                        "a hopeful"
                        "an uneasy"))
          (random-elt #("It passes after a moment."
                        "It lingers for a while."))))

(defun weather ()
  (if (randomp)
    (format nil "A ~A ~A the ~A air against your skin."
            (random-elt #("light breeze"
                          "gentle breeze"
                          "stiff wind"
                          "strong wind"))
            (random-elt #("moves" "pushes"))
            (random-elt #("hot" "warm" "sticky" "humid" "wet")))
    (random-elt #("It begins to drizzle."
                  "The wind picks up, pushing you around."
                  "A cool wind blows from the north."
                  "You hear thunder rumble in the distance."
                  "A dust storm gathers in the distance."
                  "The clouds part.  The sun beats down on your back."
                  "For a moment the humidity drops.  You savor the dry air."))))

(defun random-flavor ()
  (let ((r (random 1.0)))
    (cond ((< r 0.33) (animal))
          ((< r 0.50) (feeling))
          (t          (weather)))))
