(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :define-constant
               :deletef
               :mkstr
               :once-only
               :rcurry
               :read-file-into-string
               :removef
               :symb
               :with-gensyms

               )
  :package "AP.QUICKUTILS")
