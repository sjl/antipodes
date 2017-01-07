(in-package :ap)


;;;; Data ---------------------------------------------------------------------
(defparameter *logo* (read-file-into-string "data/logo.txt"))
(defparameter *intro1* (read-file-into-string "data/intro1.txt"))
(defparameter *intro2* (read-file-into-string "data/intro2.txt"))
(defparameter *intro3* (read-file-into-string "data/intro3.txt"))
(defparameter *intro4* (read-file-into-string "data/intro4.txt"))
(defparameter *intro5* (read-file-into-string "data/intro5.txt"))
(defparameter *intro6* (read-file-into-string "data/intro6.txt"))

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)

(defparameter *width* nil)
(defparameter *height* nil)

(defparameter *terrain* nil)

;;;; Intro --------------------------------------------------------------------
(defmacro dialog (&body body)
  `(with-dims (50 10)
     (with-panel-and-window
         (pan win *width* *height*
              (center *width* *screen-width*)
              (center *height* *screen-height*))
       (charms:clear-window win)
       (border win)
       ,@body
       (write-string-right win "Press any key" -1 (1- *height*))
       (redraw)
       (charms:get-char win))))

(defun intro1 ()
  (if (eq :left (dialog (write-lines-left win *intro1* 1 1)))
    (title)
    (intro2)))

(defun intro2 ()
  (if (eq :left (dialog (write-lines-left win *intro2* 1 1)))
    (intro1)
    (intro3)))

(defun intro3 ()
  (if (eq :left (dialog (write-lines-left win *intro3* 1 1)))
    (intro2)
    (intro4)))

(defun intro4 ()
  (if (eq :left (dialog (write-lines-left win *intro4* 1 1)))
    (intro3)
    (intro5)))

(defun intro5 ()
  (if (eq :left (dialog (write-lines-left win *intro5* 1 1)))
    (intro4)
    (intro6)))

(defun intro6 ()
  (if (eq :left (dialog (write-lines-left win *intro6* 1 1)))
    (intro5)
    (generate-world)))


;;;; Title --------------------------------------------------------------------
(defun title ()
  (with-dims (50 10)
    (with-panel-and-window
        (pan win *width* *height*
             (center *width* *screen-width*)
             (center *height* *screen-height*))
      (write-lines-centered win *logo* 0)
      (redraw)
      (charms:get-char win)))
  (intro1))


;;;; World Generation ---------------------------------------------------------
(defun generate-world% ()
  (setf *terrain* (ap.gen::generate-heightmap)))

(defun generate-world ()
  (with-dims (20 2)
    (with-panel-and-window
        (pan win *width* *height*
             (center *width* *screen-width*)
             (center *height* *screen-height*))
      (write-string-centered win "Generating world..." 0)
      (redraw)
      (generate-world%)))
  (world-map))


;;;; World Map ----------------------------------------------------------------
(defun terrain-char (height)
  (cond ((< height 0.0) #\~)
        (t #\.)))

(defun render-map (window)
  (iterate
    (with terrain = *terrain*)
    (for-nested ((sx :from 0 :below *width*)
                 (sy :from 0 :below *height*)))
    (for glyph = (terrain-char (aref terrain sx sy)))
    (charms:write-char-at-point window glyph sx sy)))

(defun world-map ()
  (with-dims ((- *screen-width* 2) (- *screen-height* 2))
    (with-panel-and-window (map-pan map-win *width* *height* 0 0)
      (iterate
        (render-map map-win)
        (redraw)
        (until (eql #\q (charms:get-char map-win))))))
  nil)


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
