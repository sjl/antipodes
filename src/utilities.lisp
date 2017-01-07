(in-package :ap.utilities)

;;;; Charms -------------------------------------------------------------------
(defmacro with-window ((symbol width height x y) &body body)
  `(let ((,symbol (charms:make-window ,width ,height ,x ,y)))
     (charms:enable-extra-keys ,symbol)
     (unwind-protect (progn ,@body)
       (charms:destroy-window ,symbol))))

(defmacro with-panel ((symbol window) &body body)
  `(let ((,symbol (charms:make-panel ,window)))
     (unwind-protect (progn ,@body)
       (charms:destroy-panel ,symbol))))

(defmacro with-windows (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-window ,(first bindings)
       (with-windows ,(rest bindings)
         ,@body))))

(defmacro with-panels (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-panel ,(first bindings)
       (with-panels ,(rest bindings)
         ,@body))))

(defmacro with-panel-and-window
    ((panel-symbol window-symbol width height x y) &body body)
  `(with-window (,window-symbol ,width ,height ,x ,y)
     (with-panel (,panel-symbol ,window-symbol)
       ,@body)))

(defmacro with-panels-and-windows (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-panel-and-window ,(first bindings)
       (with-panels-and-windows ,(rest bindings)
         ,@body))))


(defun border (window)
  (charms::check-status
    (charms/ll:wborder (charms::window-pointer window) 0 0 0 0 0 0 0 0))
  t)

(defun redraw ()
  (charms:update-panels)
  (charms:update))


(defun write-string-left (window string x y)
  (charms:write-string-at-point window string x y))

(defun write-string-right (window string x-offset y)
  (charms:write-string-at-point
    window string
    (- ap::*width* (length string) (abs x-offset))
    y))

(defun write-string-centered (window string y)
  (charms:write-string-at-point window string (center (length string) ap::*width*) y))

(defun write-lines-left (window contents start-x start-y)
  (if (stringp contents)
    (write-lines-left window (cl-strings:split contents #\newline) start-x start-y)
    (iterate (for line :in contents)
             (for y :from start-y)
             (write-string-left window line start-x y))))

(defun write-lines-centered (window contents start-y)
  (if (stringp contents)
    (write-lines-centered window (cl-strings:split contents #\newline) start-y)
    (iterate (for line :in contents)
             (for y :from start-y)
             (write-string-centered window line y))))


(defmacro with-dims ((width height) &body body)
  `(let ((ap::*width* ,width)
         (ap::*height* ,height))
     ,@body))

(defmacro with-window-dims (window &body body)
  (with-gensyms (w h)
    `(multiple-value-bind (,w ,h) (charms:window-dimensions ,window)
       (with-dims (,w ,h)
         ,@body))))

(defmacro defcolors (&rest colors)
  `(progn
    ,@(iterate (for n :from 0)
               (for (constant nil nil) :in colors)
               (collect `(define-constant ,constant ,n)))
    (defun init-colors ()
      ,@(iterate
          (for (constant fg bg) :in colors)
          (collect `(charms/ll:init-pair ,constant ,fg ,bg))))))

(defmacro with-color ((window color) &body body)
  (once-only (window color)
    `(unwind-protect
       (progn
         (charms/ll:wattron (charms::window-pointer ,window)
                            (charms/ll:color-pair ,color))
         ,@body)
       (charms/ll:wattroff (charms::window-pointer ,window)
                           (charms/ll:color-pair ,color)))))


;;;; Maths --------------------------------------------------------------------
(defun center (size max)
  (truncate (- max size) 2))


;;;; File I/O -----------------------------------------------------------------
(defun read-file-into-form (filename)
  (with-open-file (s filename)
    (read s)))
