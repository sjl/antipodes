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

(defparameter *view-x* nil)
(defparameter *view-y* nil)

(defparameter *wat* nil)
(defparameter *player* nil)
(defparameter *sidebar-width* 20)


;;;; More Utils Lol
(defmacro with-window-dims (window &body body)
  (with-gensyms (w h)
    `(multiple-value-bind (,w ,h) (charms:window-dimensions ,window)
       (with-dims (,w ,h)
         ,@body))))


;;;; Colors -------------------------------------------------------------------
(defcolors
  (+white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
  (+blue-black+   charms/ll:COLOR_BLUE    charms/ll:COLOR_BLACK)
  (+cyan-black+   charms/ll:COLOR_CYAN    charms/ll:COLOR_BLACK)
  (+yellow-black+ charms/ll:COLOR_YELLOW  charms/ll:COLOR_BLACK)
  (+green-black+  charms/ll:COLOR_GREEN   charms/ll:COLOR_BLACK)
  (+pink-black+   charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)

  (+black-white+  charms/ll:COLOR_BLACK   charms/ll:COLOR_WHITE)
  )


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
  ; (intro1)
  (generate-world)
  )


;;;; World Generation ---------------------------------------------------------
(defun generate-world% ()
  (setf *terrain* (ap.generation::generate-heightmap))
  (destructuring-bind (map-width map-height) (array-dimensions *terrain*)
    (setf *view-x* (truncate map-width 2)
          *view-y* (truncate map-height 2)))
  (setf *player* (make-player)))

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
  (cond ((< height -0.20) (values #\~ +blue-black+)) ; deep water
        ((< height -0.05) (values #\~ +cyan-black+)) ; shallow water
        ((< height  0.05) (values #\` +yellow-black+)) ; sand
        ((< height  0.40) (values #\. +white-black+)) ; dirt
        ((< height  0.55) (values #\^ +white-black+)) ; hills
        (t                (values #\# +white-black+)))) ; mountains

(defun clamp-view (coord size)
  (clamp 0 (- ap.generation::*map-size* size 1) coord))

(defun center-view (width height x y)
  (setf *view-x* (clamp-view (- x (truncate width 2)) width)
        *view-y* (clamp-view (- y (truncate height 2)) height)))

(defun center-view-on-player (width height)
  (center-view width height
               (coords/x *player*)
               (coords/y *player*)))

(defun render-map (window)
  (iterate
    (with terrain = *terrain*)
    (with vx = *view-x*)
    (with vy = *view-y*)
    (for-nested ((sx :from 0 :below *width*)
                 (sy :from 0 :below *height*)))
    (for x = (+ sx vx))
    (for y = (+ sy vy))
    (for (values glyph color) = (terrain-char (aref terrain x y)))
    (with-color (window color)
      (charms:write-char-at-point window glyph sx sy))
    (for entity = (find-if #'visible? (coords-lookup x y)))
    (when entity
      (with-color (window (visible/color entity))
        (charms:write-string-at-point window (visible/glyph entity) sx sy)))))


(defun world-map-input (window)
  (case (charms:get-char window)
    (#\q :quit)
    (:left  (zapf *view-x* (clamp (1- %) 0 20000)))
    (:right (zapf *view-x* (clamp (1+ %) 0 20000)))
    (:up    (zapf *view-y* (clamp (1- %) 0 20000)))
    (:down  (zapf *view-y* (clamp (1+ %) 0 20000)))))

(defun world-map ()
  (with-dims ((- *screen-width* 2) (- *screen-height* 2))
    (with-panels-and-windows
        ((map-pan map-win (- *width* *sidebar-width*) *height* 0 0)
         (bar-pan bar-win *sidebar-width* *height* (- *width* *sidebar-width*) 0))
      (iterate
        (charms:clear-window bar-win)
        (border bar-win)
        (with-window-dims map-win
          (center-view-on-player *width* *height*)
          (render-map map-win))
        (redraw)
        (until (eql :quit (world-map-input bar-win))))))
  nil)


;;;; Main ---------------------------------------------------------------------
(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys t)
    (charms/ll:start-color)
    (charms:clear-window t)
    (init-colors)

    ; todo: handle resizes
    (setf (values *screen-width* *screen-height*)
          (charms:window-dimensions t))

    (let ((*width* *screen-width*)
          (*height* *screen-height*))
      (title)))

  t)
