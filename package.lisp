(defpackage :ap.utilities
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :beast
    :ap.quickutils)
  (:export
    :with-window
    :with-windows
    :with-panels
    :with-panel
    :with-panel-and-window
    :with-panels-and-windows
    :center
    :border
    :redraw
    :write-string-left
    :write-string-right
    :write-string-centered
    :write-lines-left
    :write-lines-centered
    :with-dims
    :defcolors
    :with-color
    :init-colors
    ))

(defpackage :ap.generation
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :beast
    :ap.utilities
    :ap.quickutils)
  (:export))

(defpackage :ap.entities
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :beast
    :ap.utilities
    :ap.quickutils)
  (:export
    :player
    :make-player
    :player/health
    :health-description
    :player/energy
    :energy-description

    :coords
    :coords/x
    :coords/y
    :coords?
    :coords-lookup
    :coords-move-entity

    :visible
    :visible?
    :visible/glyph
    :visible/color
    ))

(defpackage :ap
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :beast
    :ap.entities
    :ap.utilities
    :ap.quickutils)
  (:export
    :main))
