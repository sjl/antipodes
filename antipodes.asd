(asdf:defsystem :antipodes
  :description "IGI Game Jam 2017"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:beast
               :black-tie
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
                 (:file "config")
                 (:module "aspects" :serial t
                  :components ((:file "coordinates")
                               (:file "holdable")
                               (:file "trigger")
                               (:file "worth")
                               (:file "visible")))
                 (:module "entities" :serial t
                  :components ((:file "food")
                               (:file "clothing")
                               (:file "jewelery")
                               (:file "ruin")
                               (:file "player")))
                 (:file "flavor")
                 (:file "main")))))
