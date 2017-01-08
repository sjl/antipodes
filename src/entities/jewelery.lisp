(in-package :ap.entities)

(defparameter *gemstones*
  (read-file-into-form "data/gemstones.lisp"))

(defparameter *jewelery*
  #("earring"
    "ring"
    "bracelet"
    "necklace"
    "ankle bracelet"
    "barette"
    "pin"
    "brooch"
    "hat pin"
    "cuff link"
    "locket"
    "medallion"))

(define-entity jewelery (visible coords holdable))

(defun random-jewelery-description ()
  (destructuring-bind (article gem)
      (random-elt *gemstones*)
    (format nil "~A ~A ~A" article gem (random-elt *jewelery*))))

(defun make-jewelery (x y)
  (create-entity 'jewelery
    :coords/x x
    :coords/y y
    :visible/glyph "*"
    :visible/color ap::+black-pink+
    :holdable/description (random-jewelery-description)))
