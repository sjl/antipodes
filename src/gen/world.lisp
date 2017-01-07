(in-package :ap.gen)


(define-constant +chunk-size+ (expt 2 8))

(defun make-empty-heightmap ()
  (make-array (list +chunk-size+ +chunk-size+)
    :element-type 'single-float
    :initial-element 0.0))

(defun noise-heightmap (heightmap)
  (iterate
    (for (val x y) :in-array heightmap)
    (setf (aref heightmap x y)
          (black-tie:perlin-noise-single-float
            (* x 0.1)
            (* y 0.1)
            0.0))))

(defun generate-heightmap ()
  (let ((heightmap (make-empty-heightmap)))
    (noise-heightmap heightmap)
    heightmap))
