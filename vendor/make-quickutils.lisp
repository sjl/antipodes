(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :define-constant
               :mkstr
               :once-only
               :rcurry
               :read-file-into-string
               :symb
               :with-gensyms

               )
  :package "AP.QUICKUTILS")
