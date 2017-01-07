(in-package :ap.gen)


(defparameter *map-size* 2000)
(defparameter *noise-scale* 0.03)
(defparameter *noise-seed-x* (random 1000.0))
(defparameter *noise-seed-y* (random 1000.0))

(defun make-empty-heightmap ()
  (make-array (list *map-size* *map-size*)
    :element-type 'single-float
    :initial-element 0.0))

(defun noise-heightmap (heightmap)
  (iterate
    (with ox = *noise-seed-x*)
    (with oy = *noise-seed-x*)
    (with scale = *noise-scale*)
    (for (val x y) :in-array heightmap)
    (setf (aref heightmap x y)
          (black-tie:perlin-noise-single-float
            (+ ox (* x scale))
            (+ oy (* y scale))
            0.0))))

(defun generate-heightmap ()
  (let ((heightmap (make-empty-heightmap)))
    (noise-heightmap heightmap)
    heightmap))
