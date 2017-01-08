(in-package :ap.entities)

(defparameter *foods*
  (concatenate 'vector
               (read-file-into-form "data/vegetables.lisp")
               (read-file-into-form "data/meat.lisp")))

(defparameter *containers*
  #(("can"                "")
    ("can"                "preserved ")
    ("can"                "old ")
    ("can"                "pickled ")
    ("tin"                "")
    ("package"            "")
    ("shrink-wrapped bag" "")
    ("vacuum-sealed bag"  "")
    ("jar"                "")
    ("jar"                "preserved ")
    ("jar"                "pickled ")
    ("ration"             "")
    ("box"                "dried ")
    ("box"                "dehydrated ")
    ("bag"                "dried ")
    ("bag"                "dehydrated ")
    ("sack"               "dried ")
    ("sack"               "dehydrated ")
    ("carton"             "dried ")
    ("carton"             "dehydrated ")
    ("bottle"             "slurried ")
    ("jug"                "slurried ")
    ))

(defparameter *tastes*
  #("a bit rotten"
    "a bit strange"
    "awful"
    "better than nothing"
    "better than you expected"
    "crunchy"
    "decent"
    "delicious"
    "depressing"
    "disgusting"
    "expensive"
    "partially fermented"
    "faintly of mice"
    "gritty"
    "horrifying"
    "like an old sock"
    "like it's brand new"
    "like it's starting to go bad"
    "like something else"
    "like something your father used to make"
    "like something your mother used to make"
    "musty"
    "okay"
    "pretty nice"
    "questionable"
    "salty"
    "sour"
    "spicy"
    "wonderful"))


(define-entity food (visible coords holdable)
  (energy :accessor food/energy :initarg :food/energy))

(defun random-food-energy ()
  (random-range 30.0 80.0))

(defun random-food-description ()
  (destructuring-bind (container prefix)
      (random-elt *containers*)
    (format nil "a ~A of ~A~A"
            container
            prefix
            (random-elt *foods*))))

(defun random-food-taste ()
  (format nil "It tastes ~A."
          (random-elt *tastes*)))

(defun make-food (x y)
  (create-entity 'food
    :coords/x x
    :coords/y y
    :visible/glyph "%"
    :visible/color ap::+black-yellow+
    :holdable/description (random-food-description)
    :food/energy (random-food-energy)))

