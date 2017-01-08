(in-package :ap.entities)

(defparameter *foods*
  (concatenate 'vector
               (read-file-into-form "data/vegetables.lisp")
               (read-file-into-form "data/meat.lisp")))


(define-entity food (visible coords holdable)
  (energy :accessor food/energy :initarg :food/energy))

(defun random-food-energy ()
  (random-range 30.0 80.0))

(defun random-food-description ()
  (format nil "a ~A of ~A"
          (random-elt #("can" "tin" "package"))
          (random-elt *foods*)))

(defun random-food-taste ()
  (format nil "It tastes ~A."
          (random-elt #("delicious"
                        "okay"
                        "wonderful"
                        "decent"
                        "musty"
                        "salty"
                        "awful"
                        "better than nothing"
                        "questionable"
                        "pretty nice"
                        "expensive"
                        "horrifying"
                        "like mice"
                        "like an old sock"))))

(defun make-food (x y)
  (create-entity 'food
    :coords/x x
    :coords/y y
    :visible/glyph "%"
    :visible/color ap::+black-yellow+
    :holdable/description (random-food-description)
    :food/energy (random-food-energy)))

