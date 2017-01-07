(in-package :ap.entities)

(defparameter *world-contents*
  (make-array (list ap.generation::*map-size*
                    ap.generation::*map-size*)
    :initial-element nil))

(define-aspect coords x y)

(defun within-bounds-p (x y)
  (and (in-range-p 0 x ap.generation::*map-size*)
       (in-range-p 0 y ap.generation::*map-size*)))

(defun coords-insert-entity (e)
  (push e (aref *world-contents* (coords/x e) (coords/y e))))

(defun coords-remove-entity (e)
  (deletef (aref *world-contents* (coords/x e) (coords/y e)) e))

(defun coords-move-entity (e new-x new-y)
  (when (within-bounds-p new-x new-y)
    (coords-remove-entity e)
    (setf (coords/x e) new-x
          (coords/y e) new-y)
    (coords-insert-entity e)))

(defun coords-lookup (x y)
  (when (within-bounds-p x y)
    (aref *world-contents* x y)))

(defun nearby (entity &optional (radius 1))
  (remove entity
          (iterate (with x = (coords/x entity))
                   (with y = (coords/y entity))
                   (for (dx dy) :within-radius radius)
                   (appending (coords-lookup (+ x dx)
                                             (+ y dy))))))

(defmethod entity-created :after ((entity coords))
  (coords-insert-entity entity))

(defmethod entity-destroyed :after ((entity coords))
  (coords-remove-entity entity))
