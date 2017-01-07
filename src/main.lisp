(in-package :ap)


;;;; Data ---------------------------------------------------------------------
(defparameter *logo* (read-file-into-string "data/logo.txt"))

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)

(defparameter *width* nil)
(defparameter *height* nil)


;;;; State Machine ------------------------------------------------------------
(defun title ()
  (with-dims (50 10)
    (with-panel-and-window
        (pan win *width* *height*
             (center *width* *screen-width*)
             (center *height* *screen-height*))
      (write-lines-centered win *logo* 0)
      (redraw)
      (charms:get-char win))))


;;;; Main ---------------------------------------------------------------------
(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys t)
    ; (charms:enable-non-blocking-mode t)

    ; todo: handle resizes
    (setf (values *screen-width* *screen-height*)
          (charms:window-dimensions t))

    (let ((*width* *screen-width*)
          (*height* *screen-height*))
      (title)))

  t)
