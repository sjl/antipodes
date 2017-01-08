(in-package :ap.entities)

(define-constant +inventory-size+ 10)


(defun health-description (health-value)
  (cond ((<= health-value 0.0) "dead")
        ((< health-value 10.0) "almost dead")
        ((< health-value 50.0) "in bad shape")
        ((< health-value 70.0) "feeling decent")
        ((<= health-value 100.0) "feeling healthy")))

(defun energy-description (energy-value)
  (cond ((<= energy-value 0.0) "starving")
        ((< energy-value 30.0) "famished")
        ((< energy-value 50.0) "very hungry")
        ((< energy-value 80.0) "hungry")
        ((< energy-value 95.0) "peckish")
        ((<= energy-value 100.0) "full")
        (t "stuffed")))


(define-entity player (coords visible)
  (health :accessor player/health :initform 100.0)
  (energy :accessor player/energy :initform 100.0)
  (inventory :accessor player/inventory :initform nil))


(defun make-player ()
  (create-entity 'player
    :coords/x (round (* 0.5 ap::*map-size*))
    :coords/y (round (* 0.05 ap::*map-size*))
    :visible/glyph "@"
    :visible/color ap::+black-white+))


(defun tick-player (player)
  (zapf (player/energy player) (clamp 0.0 140.0 (- % 0.3))
        (player/health player) (clamp 0.0 100.0
                                      (+ % (if (< (player/energy player) 1.0)
                                             -0.5
                                             0.1)))))


(defun player-inventory-full-p (player)
  (= +inventory-size+ (length (player/inventory player))))

(defun player-inventory-empty-p (player)
  (zerop (length (player/inventory player))))

(defun player-get (player entity)
  (assert (holdable? entity) () "Entity ~S is not holdable")
  (assert (coords? entity) () "Entity ~S has no coords")
  (assert (not (player-inventory-full-p player)) () "Player's inventory is full")
  (coords-remove-entity entity)
  (push entity (player/inventory player)))

(defun player-drop (player entity)
  (removef (player/inventory player) entity)
  (setf (coords/x entity) (coords/x player)
        (coords/y entity) (coords/y player))
  (coords-insert-entity entity))

(defun player-eat (player food)
  (incf (player/energy player)
        (food/energy food))
  (removef (player/inventory player) food)
  (destroy-entity food))
