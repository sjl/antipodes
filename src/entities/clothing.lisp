(in-package :ap.entities)

(defparameter *fabrics*
  (read-file-into-form "data/fabrics.lisp"))

(defparameter *singular-clothes*
  #("coat"
    "shirt"
    "skirt"
    "dress"
    "belt"
    "tie"
    "bow tie"
    "sock"
    "bra"
    "scarf"
    "hat"
    "sweatshirt"
    "jacket"
    "vest"
    "blouse"
    "sweater"
    "dress shirt"))

(defparameter *plural-clothes*
  #("pants"
    "slacks"
    "socks"
    "briefs"
    "pajamas"
    "gloves"
    "long johns"
    "tights"
    "shorts"
    "boxers"
    "panties"))

(define-entity clothing (visible coords holdable worth))

(defun random-clothing-description ()
  (destructuring-bind (article fabric)
      (random-elt *fabrics*)
    (if (randomp 0.7)
      (format nil "~A ~A ~A"
              article
              fabric
              (random-elt *singular-clothes*))
      (format nil "a pair of ~A ~A"
              fabric
              (random-elt *plural-clothes*)))))

(defun make-clothing (x y)
  (create-entity 'clothing
    :coords/x x
    :coords/y y
    :visible/glyph "&"
    :visible/color ap::+black-white+
    :worth/points (random-range 100 200)
    :holdable/description (random-clothing-description)))

