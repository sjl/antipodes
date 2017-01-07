(in-package :ap.entities)

(defun health-description (health-value)
  (cond ((< health-value 0.0) "dead")
        ((< health-value 10.0) "almost dead")
        ((< health-value 50.0) "in bad shape")
        ((< health-value 70.0) "feeling decent")
        ((<= health-value 100.0) "feeling healthy")))

(defun energy-description (energy-value)
  (cond ((< energy-value 0.0) "starving")
        ((< energy-value 30.0) "famished")
        ((< energy-value 50.0) "very hungry")
        ((< energy-value 70.0) "hungry")
        ((< energy-value 80.0) "peckish")
        ((<= energy-value 100.0) "full")))

(define-entity player (coords visible)
  (health :accessor player/health :initform 100.0)
  (energy :accessor player/energy :initform 100.0))

(defun make-player ()
  (create-entity 'player
                 :coords/x (round (* 0.5 ap.generation::*map-size*))
                 :coords/y (round (* 0.9 ap.generation::*map-size*))
                 :visible/glyph "@"
                 :visible/color ap::+black-white+))
