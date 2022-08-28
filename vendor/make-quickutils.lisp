(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :define-constant
               :deletef
               :read-file-into-string
               :removef

               )
  :package "AP.QUICKUTILS")
