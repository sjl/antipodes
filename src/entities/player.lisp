(in-package :ap.entities)


(define-entity player (coords visible))

(defun make-player ()
  (create-entity 'player
                 :coords/x (round (* 0.5 ap.generation::*map-size*))
                 :coords/y (round (* 0.9 ap.generation::*map-size*))
                 :visible/glyph "@"
                 :visible/color ap::+black-white+))
