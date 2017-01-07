(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :mkstr
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "AP.QUICKUTILS")
