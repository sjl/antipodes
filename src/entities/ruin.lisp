(in-package :ap.entities)

(defparameter *venues*
  (read-file-into-form "data/venues.lisp"))

(defparameter *graffiti*
  #("give up"
    "why me?"
    "tom <3 alice"
    "dave was here"
    "420"
    "goodbye"
    "5823"
    "jesus saves"
    "your next"
    "hail satan"
    "run"
    "head north"
    "head south"
    "were damned"
    "hope is lost"
    "turn back"))

(defparameter *ruin-flavor*
  #("Maybe there's something useful left."
    "You're surprised it's still standing."
    "Maybe it's worth taking a look."
    "Perhaps you should scavenge?"
    "You have a bad feeling about it."
    "It brings back fond memories."
    "The walls are crumbling."
    "Sickly-looking leaves cover its walls."
    "It looks like someone has been here recently."
    "The walls are charred from a fire."
    "A small animal scurries into a hole under the wall."
    "Your parents used to live near one of these."
    "The world has suddenly gotten quiet."
    "A remnant of a happier time."))

(define-entity ruin (coords trigger))

(defun random-ruin-text ()
  (format nil "You see the ruins of ~A.~2%~A"
          (random-elt *venues*)
          (if (randomp ap::*graffiti-chance*)
            (format nil "Someone has graffitied \"~A\" on the wall."
                    (string-upcase (random-elt *graffiti*)))
            (random-elt *ruin-flavor*))))

(defun make-ruin (x y)
  (create-entity 'ruin
    :coords/x x
    :coords/y y
    :trigger/text (random-ruin-text)))
