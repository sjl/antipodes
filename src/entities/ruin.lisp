(in-package :ap.entities)

(defparameter *venues*
  (read-file-into-form "data/venues.lisp"))

(define-entity ruin (coords trigger))

(defun random-ruin-text ()
  (format nil "You see the ruins of ~A.~2%~A"
          (random-elt *venues*)
          (if (randomp ap::*graffiti-chance*)
            (format nil "Someone has graffitied \"~A\" on the wall."
                    (string-upcase (random-elt #("give up"
                                                 "why me?"
                                                 "tom <3 alice"
                                                 "dave was here"
                                                 "420"
                                                 "goodbye"
                                                 "5823"
                                                 "jesus saves"
                                                 "hail satan"
                                                 "run"
                                                 "head north"
                                                 "head south"
                                                 "were damned"
                                                 "hope is lost"
                                                 "turn back"))))
            (random-elt #("Maybe there's something useful left."
                          "Perhaps you should scavenge?"
                          "It brings back fond memories."
                          "Your parents used to live near one of these."
                          "The world has suddenly gotten quiet."
                          "A remnant of a happier time.")))))

(defun make-ruin (x y)
  (create-entity 'ruin
    :coords/x x
    :coords/y y
    :trigger/text (random-ruin-text)))
