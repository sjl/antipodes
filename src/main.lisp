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
(defparameter *structures* nil)

(defparameter *view-x* nil)
(defparameter *view-y* nil)

(defparameter *player* nil)


;;;; Heightmap ----------------------------------------------------------------
;;; TODO: Switch to something less samey

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

(defun random-coord ()
  (random *map-size*))

(defun underwaterp (height)
  (< height -0.05))

(defun deepwaterp (height)
  (< height -0.20))


;;;; Ruins --------------------------------------------------------------------
(defun make-empty-structures ()
  (make-array (list *map-size* *map-size*)))

(defun passablep (structure)
  (if (member structure '(:wall))
    nil
    t))

(defun add-intact-ruin (width height start-x start-y)
  (iterate (for-nested ((x :from start-x :below (+ start-x width))
                        (y :from start-y :below (+ start-y height))))
           (setf (aref *structures* x y) :floor))
  (iterate (repeat width)
           (for x :from start-x)
           (setf (aref *structures* x start-y) :wall
                 (aref *structures* x (+ start-y height -1)) :wall))
  (iterate (repeat height)
           (for y :from start-y)
           (setf (aref *structures* start-x y) :wall
                 (aref *structures* (+ start-x width) y) :wall)))

(defun add-ruin-door (width height start-x start-y)
  (setf (aref *structures* (+ start-x (random width))
              (if (randomp)
                start-y
                (+ start-y height -1)))
        nil))

(defun decay-ruin (width height start-x start-y condition)
  (iterate (for-nested ((x :from start-x :to (+ start-x width))
                        (y :from start-y :below (+ start-y height))))
           (when (or (randomp condition)
                     (and (deepwaterp (aref *terrain* x y))
                          (not (eq :wall (aref *structures* x y)))))
             (setf (aref *structures* x y) nil))))

(defun random-ruin-floor-space (width height start-x start-y)
  (values (random-range (1+ start-x) (+ start-x width -1))
          (random-range (1+ start-y) (+ start-y height -1))))

(defun place-ruin-food (width height start-x start-y)
  (iterate
    (repeat (random 4))
    (multiple-value-call #'make-food
      (random-ruin-floor-space width height start-x start-y))))

(defun place-ruin-clothing (width height start-x start-y)
  (when (randomp)
    (iterate
      (repeat (random-range 1 4))
      (multiple-value-call #'make-clothing
        (random-ruin-floor-space width height start-x start-y)))))

(defun add-ruin-trigger (width height start-x start-y)
  (make-ruin (+ start-x (truncate width 2))
             (+ start-y (truncate height 2))))

(defun add-ruin ()
  (let ((x (clamp 0 (- *map-size* 50) (random-coord)))
        (y (clamp 0 (- *map-size* 50) (random-coord)))
        (width (max 5 (truncate (random-gaussian *ruin-size-mean* *ruin-size-dev*))))
        (height (max 5 (truncate (random-gaussian *ruin-size-mean* *ruin-size-dev*))))
        (condition (random-range 0.1 1.0)))
    (add-intact-ruin width height x y)
    (add-ruin-door width height x y)
    (decay-ruin width height x y condition)
    (place-ruin-food width height x y)
    (place-ruin-clothing width height x y)
    (add-ruin-trigger width height x y)))

(defun fill-ruins ()
  (iterate
    (repeat (round (* *ruin-density* *map-size* *map-size*)))
    (add-ruin)))


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
  (intro1)
  ; (generate-world)
  )


;;;; World Generation ---------------------------------------------------------
(defun generate-terrain ()
  (setf *terrain* (generate-heightmap)
        *view-x* 0 *view-y* 0))

(defun spawn-player ()
  (setf *player* (make-player))
  (iterate (repeat 2)
           (player-get *player* (make-clothing 0 0))))

(defun place-things (density constructor)
  (iterate
    (with remaining = (round (* density
                                *map-size*
                                *map-size*)))
    (until (zerop remaining))
    (for x = (random-coord))
    (for y = (random-coord))
    (when (not (underwaterp (aref *terrain* x y)))
      (funcall constructor x y)
      (decf remaining))))

(defun generate-structures ()
  (setf *structures* (make-empty-structures))
  (fill-ruins))

(defun generate-world ()
  (clear-entities)
  (with-dims (30 (+ 2 5))
    (with-panel-and-window
        (pan win *width* *height*
             (center *width* *screen-width*)
             (center *height* *screen-height*))
      (border win)
      (progn (write-string-left win "Generating terrain..." 1 1)
             (redraw)
             (generate-terrain))
      (progn (write-string-left win "Generating structures..." 1 2)
             (redraw)
             (generate-structures))
      (progn (write-string-left win "Placing food..." 1 3)
             (redraw)
             (place-things *food-density* #'make-food))
      (progn (write-string-left win "Placing items..." 1 4)
             (redraw)
             (place-things *clothing-density* #'make-clothing)
             (place-things *jewelery-density* #'make-jewelery))
      (progn (write-string-left win "Spawning player..." 1 5)
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
        (t                (values #\* +white-black+))))

(defun structure-char (contents)
  (case contents
    (:wall #\#)
    (:floor #\_)))


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
  (let* ((x (coords/x *player*))
         (y (coords/y *player*))
         (items (-<> (coords-lookup x y)
                  (remove-if-not #'holdable? <>)))
         (here-string (if (underwaterp (aref *terrain* x y))
                        "floating here"
                        "here")))
    (when items
      (if (= (length items) 1)
        (write-string-left
          window
          (format nil "You see ~A ~A"
                  (holdable/description (first items))
                  here-string)
          0 0)
        (progn
          (write-string-left window (format nil "The following things are ~A:"
                                            here-string)
                             0 0)
          (iterate
            (for item :in items)
            (for y :from 1)
            (write-string-left window
                               (format nil "  ~A" (holdable/description item))
                               0 1)))))))

(defun render-map (window)
  (iterate
    (with terrain = *terrain*)
    (with structures = *structures*)
    (with vx = *view-x*)
    (with vy = *view-y*)
    (for-nested ((sx :from 0 :below (1- *width*))
                 (sy :from 0 :below (1- *height*))))
    (for x = (+ sx vx))
    (for y = (+ sy vy))

    (for (values terrain-glyph terrain-color) = (terrain-char (aref terrain x y)))
    (with-color (window terrain-color)
      (charms:write-char-at-point window terrain-glyph sx sy))

    (for structure-glyph = (structure-char (aref structures x y)))
    (when structure-glyph
      (charms:write-char-at-point window structure-glyph sx sy))

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
  (let* ((player *player*)
         (dest-x (+ (coords/x player) dx))
         (dest-y (+ (coords/y player) dy)))
    (when (passablep (aref *structures* dest-x dest-y))
      (coords-move-entity player dest-x dest-y))))

(defun get-items ()
  (iterate (for item :in (remove-if-not #'holdable? (coords-nearby *player* 0)))
           (when (player-inventory-full-p *player*)
             (popup "You can't carry any more items.")
             (return))
           (player-get *player* item)))


(defun world-map-input (window)
  (case (charms:get-char window)
    (#\q :quit)
    (#\h :help)
    (#\g (get-items) :tick)
    (:left  (move-player -1 0) :tick)
    (:right (move-player 1 0) :tick)
    (:up    (move-player 0 -1) :tick)
    (:down  (move-player 0 1) :tick)))


(defun check-triggers ()
  (iterate (for trigger :in (-<> *player*
                              (coords-nearby <> 10)
                              (remove-if-not #'trigger? <>)))
           (popup (trigger/text trigger))
           (destroy-entity trigger)))

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
          (popup (format nil "You must head north to survive.~2%You can press h for help in-game."))
          (if (ap.flavor:flavorp)
            (popup (ap.flavor:random-flavor))
            (case (world-map-input bar-win)
              (:tick (tick-player *player*)
                     (check-triggers))
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

