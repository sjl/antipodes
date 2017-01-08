(in-package :ap)

(defparameter *sidebar-width* 30)
(defparameter *food-density* 1/10000)
(defparameter *clothing-density* 1/20000)
(defparameter *jewelery-density* 1/20000)
(defparameter *map-size* 2000)
(defparameter *noise-scale* 0.03)
(defparameter *noise-seed-x* (random 1000.0))
(defparameter *noise-seed-y* (random 1000.0))

(defparameter *ruin-density* 1/20000)
(defparameter *ruin-size-mean* 10.0)
(defparameter *ruin-size-dev* 2.0)
(defparameter *graffiti-chance* 1/10)
(defparameter *flavor-chance* 1/300)
(defparameter *flavor-cooldown* 200)

(defcolors
  (+white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
  (+blue-black+   charms/ll:COLOR_BLUE    charms/ll:COLOR_BLACK)
  (+cyan-black+   charms/ll:COLOR_CYAN    charms/ll:COLOR_BLACK)
  (+yellow-black+ charms/ll:COLOR_YELLOW  charms/ll:COLOR_BLACK)
  (+green-black+  charms/ll:COLOR_GREEN   charms/ll:COLOR_BLACK)
  (+red-black+    charms/ll:COLOR_RED     charms/ll:COLOR_BLACK)
  (+pink-black+   charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)

  (+black-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)
  (+black-blue+   charms/ll:COLOR_BLACK charms/ll:COLOR_BLUE)
  (+black-cyan+   charms/ll:COLOR_BLACK charms/ll:COLOR_CYAN)
  (+black-yellow+ charms/ll:COLOR_BLACK charms/ll:COLOR_YELLOW)
  (+black-green+  charms/ll:COLOR_BLACK charms/ll:COLOR_GREEN)
  (+black-pink+   charms/ll:COLOR_BLACK charms/ll:COLOR_MAGENTA))
