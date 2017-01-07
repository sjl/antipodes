(in-package :ap)


;;;; Data ---------------------------------------------------------------------
(defparameter *logo* (read-file-into-string "data/logo.txt"))
(defparameter *intro1* (read-file-into-string "data/intro1.txt"))
(defparameter *intro2* (read-file-into-string "data/intro2.txt"))
(defparameter *intro3* (read-file-into-string "data/intro3.txt"))
(defparameter *intro4* (read-file-into-string "data/intro4.txt"))
(defparameter *intro5* (read-file-into-string "data/intro5.txt"))
(defparameter *intro6* (read-file-into-string "data/intro6.txt"))
(defparameter *help* (read-file-into-string "data/help.txt"))

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)

(defparameter *width* nil)
(defparameter *height* nil)

(defparameter *terrain* nil)

(defparameter *view-x* nil)
(defparameter *view-y* nil)

(defparameter *wat* nil)
(defparameter *player* nil)
(defparameter *sidebar-width* 30)
(defparameter *food-density* 1/6000)


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
(defun underwaterp (height)
  (< height 0.05))

(defun generate-terrain ()
  (setf *terrain* (generate-heightmap)
        *view-x* 0 *view-y* 0))

(defun spawn-player ()
  (setf *player* (make-player)))

(defun place-food ()
  (iterate
    (with remaining = (round (* *food-density*
                                *map-size*
                                *map-size*)))
    (until (zerop remaining))
    (for x = (random *map-size*))
    (for y = (random *map-size*))
    (when (not (underwaterp (aref *terrain* x y)))
      (make-food x y)
      (decf remaining))))

(defun generate-world ()
  (clear-entities)
  (with-dims (30 (+ 2 3))
    (with-panel-and-window
        (pan win *width* *height*
             (center *width* *screen-width*)
             (center *height* *screen-height*))
      (border win)
      (progn (write-string-left win "Generating terrain..." 1 1)
             (redraw)
             (generate-terrain))
      (progn (write-string-left win "Placing food..." 1 2)
             (redraw)
             (place-food))
      (progn (write-string-left win "Spawning player..." 1 3)
             (redraw)
             (spawn-player))))
  (world-map))


;;;; Popups -------------------------------------------------------------------
(defun popup (contents)
  (let ((lines (cl-strings:split contents #\newline)))
    (with-dims ((+ 3 (apply #'max 13 (mapcar #'length lines)))
                (+ 3 (length lines)))
      (with-panel-and-window
          (pan win *width* *height*
               (center *width* *screen-width*)
               (center *height* *screen-height*))
        (charms:clear-window win)
        (border win)
        (write-lines-left win lines 1 1)
        (write-string-centered win "Press space" (1- *height*))
        (redraw)
        (iterate (until (eql #\space (charms:get-char win))))))))


;;;; World Map ----------------------------------------------------------------
(defun terrain-char (height)
  (cond ((< height -0.20) (values #\~ +blue-black+)) ; deep water
        ((< height -0.05) (values #\~ +cyan-black+)) ; shallow water
        ((< height  0.05) (values #\` +yellow-black+)) ; sand
        ((< height  0.40) (values #\. +white-black+)) ; dirt
        ((< height  0.55) (values #\^ +white-black+)) ; hills
        (t                (values #\# +white-black+)))) ; mountains

(defun clamp-view (coord size)
  (clamp 0 (- *map-size* size 1) coord))

(defun center-view (width height x y)
  (setf *view-x* (clamp-view (- x (truncate width 2)) width)
        *view-y* (clamp-view (- y (truncate height 2)) height)))

(defun center-view-on-player (width height)
  (center-view width height
               (coords/x *player*)
               (coords/y *player*)))

(defun render-items (window)
  (let ((items (-<> (coords-lookup (coords/x *player*)
                                   (coords/y *player*))
                 (remove-if-not #'holdable? <>))))
    (when items
      (if (= (length items) 1)
        (write-string-left
          window
          (format nil "You see ~A here" (holdable/description (first items)))
          0 0)
        (progn
          (write-string-left window "The following things are here:" 0 0)
          (iterate
            (for item :in items)
            (for y :from 1)
            (write-string-left window
                               (format nil "  ~A" (holdable/description item))
                               0 1)))))))

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
    (for entities = (coords-lookup x y))
    (for entity = (if (member *player* entities)
                    *player*
                    (find-if #'visible? entities)))
    (when entity
      (with-color (window (visible/color entity))
        (charms:write-string-at-point window (visible/glyph entity) sx sy)))))


(defun render-sidebar (window)
  (charms:clear-window window)
  (border window)
  (let ((p *player*))
    (write-string-left window (format nil "You are ~A" (health-description
                                                         (player/health p)))
                       1 1)
    (write-string-left window (format nil "        ~A" (energy-description
                                                         (player/energy p)))
                       1 2)
    (write-string-left window (format nil "You are carrying:") 1 4)
    (if (player-inventory-empty-p p)
      (write-string-left window (format nil "Nothing") 3 5)
      (iterate
        (for item :in (player/inventory p))
        (for y :from 5)
        (write-lines-left window
                          (cl-strings:shorten (holdable/description item)
                                              (- *width* 2 2 3 1))
                          3 y)))
    (write-string-left window (format nil "Press h for help") 1 (1- *height*))))


(defun move-player (dx dy)
  (let ((player *player*))
    (coords-move-entity player
                        (+ (coords/x player) dx)
                        (+ (coords/y player) dy))))

(defun world-map-input (window)
  (case (charms:get-char window)
    (#\q :quit)
    (#\h :help)
    (:left  (move-player -1 0) :tick)
    (:right (move-player 1 0) :tick)
    (:up    (move-player 0 -1) :tick)
    (:down  (move-player 0 1) :tick)))


(defun world-map ()
  (with-dims ((- *screen-width* 2) (- *screen-height* 1))
    (with-panels-and-windows
        ((map-pan map-win (- *width* *sidebar-width*) *height* 0 0)
         (bar-pan bar-win *sidebar-width* *height* (- *width* *sidebar-width*) 0))
      (iterate
        (with-window-dims bar-win
          (render-sidebar bar-win))
        (with-window-dims map-win
          (center-view-on-player *width* *height*)
          (render-map map-win)
          (render-items map-win))
        (redraw)
        (if-first-time
          (popup "Head north!")
          (if (ap.flavor:flavorp)
            (popup (ap.flavor:random-flavor))
            (case (world-map-input bar-win)
              (:tick (tick-player *player*))
              (:quit (return))
              (:help (popup *help*))))))))
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
