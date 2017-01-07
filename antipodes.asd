(asdf:defsystem :antipodes
  :description "IGI Game Jam 2017"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:beast
               :black-tie
               :cl-arrows
               :cl-charms
               :cl-strings
               :iterate
               :losh)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "utilities")
                 (:module "generation" :serial t
                  :components ((:file "world")))
                 (:module "aspects" :serial t
                  :components ((:file "coordinates")
                               (:file "visible")))
                 (:module "entities" :serial t
                  :components ((:file "player")))
                 (:file "main")))))
