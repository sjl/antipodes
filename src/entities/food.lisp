(in-package :ap.entities)

(defparameter *vegetables*
  (read-file-into-form "data/vegetables.lisp"))


(define-entity food (visible coords holdable)
  (energy :accessor food/energy :initarg :food/energy))

(defun random-food-energy ()
  (random-range 30.0 80.0))

(defun random-food-description ()
  (format nil "a ~A of ~A"
          (random-elt #("can" "tin" "package"))
          (random-elt *vegetables*)))

(defun make-food (x y)
  (create-entity 'food
    :coords/x x
    :coords/y y
    :visible/glyph "%"
    :visible/color ap::+yellow-black+
    :holdable/description (random-food-description)
    :food/energy (random-food-energy)))

