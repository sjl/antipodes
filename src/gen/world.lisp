(in-package :ap.gen)


(define-constant +chunk-size+ (expt 2 8))

(defun make-empty-heightmap ()
  (make-array (list +chunk-size+ +chunk-size+)
    :element-type 'single-float
    :initial-element 0.0))

(defun generate-heightmap ()
  (make-empty-heightmap))
