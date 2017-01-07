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
    :with-window-dims
    :defcolors
    :with-color
    :init-colors
    :read-file-into-form
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
    :tick-player
    :player/health
    :health-description
    :player/energy
    :energy-description
    :player/inventory
    :player-inventory-full-p
    :player-inventory-empty-p
    :player-get
    :player-drop

    :food
    :make-food
    :food/energy

    :coords
    :coords/x
    :coords/y
    :coords?
    :coords-lookup
    :coords-move-entity

    :holdable
    :holdable?
    :holdable/description

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
