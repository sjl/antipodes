;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:DEFINE-CONSTANT :DELETEF :READ-FILE-INTO-STRING :REMOVEF) :ensure-package T :package "AP.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "AP.QUICKUTILS")
    (defpackage "AP.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "AP.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:DEFINE-CONSTANT :DELETEF :MAKE-GENSYM-LIST :ONCE-ONLY
                                         :WITH-OPEN-FILE* :WITH-INPUT-FROM-FILE
                                         :READ-FILE-INTO-STRING :REMOVEF))))

  (defun %reevaluate-constant (name value test)
    (if (not (boundp name))
        value
        (let ((old (symbol-value name))
              (new value))
          (if (not (constantp name))
              (prog1 new
                (cerror "Try to redefine the variable as a constant."
                        "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
              (if (funcall test old new)
                  old
                  (restart-case
                      (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                    (ignore ()
                      :report "Retain the current value."
                      old)
                    (continue ()
                      :report "Try to redefine the constant."
                      new)))))))

  (defmacro define-constant (name initial-value &key (test ''eql) documentation)
    "Ensures that the global variable named by `name` is a constant with a value
that is equal under `test` to the result of evaluating `initial-value`. `test` is a
function designator that defaults to `eql`. If `documentation` is given, it
becomes the documentation string of the constant.

Signals an error if `name` is already a bound non-constant variable.

Signals an error if `name` is already a constant variable whose value is not
equal under `test` to result of evaluating `initial-value`."
    `(defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
       ,@(when documentation `(,documentation))))
  

  (declaim (inline delete/swapped-arguments))
  (defun delete/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'delete item sequence keyword-arguments))

  (define-modify-macro deletef (item &rest remove-keywords)
    delete/swapped-arguments
    "Modify-macro for `delete`. Sets place designated by the first argument to
the result of calling `delete` with `item`, place, and the `keyword-arguments`.")
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defmacro with-open-file* ((stream filespec &key direction element-type
                                                   if-exists if-does-not-exist external-format)
                             &body body)
    "Just like `with-open-file`, but `nil` values in the keyword arguments mean to use
the default value specified for `open`."
    (once-only (direction element-type if-exists if-does-not-exist external-format)
      `(with-open-stream
           (,stream (apply #'open ,filespec
                           (append
                            (when ,direction
                              (list :direction ,direction))
                            (when ,element-type
                              (list :element-type ,element-type))
                            (when ,if-exists
                              (list :if-exists ,if-exists))
                            (when ,if-does-not-exist
                              (list :if-does-not-exist ,if-does-not-exist))
                            (when ,external-format
                              (list :external-format ,external-format)))))
         ,@body)))
  

  (defmacro with-input-from-file ((stream-name file-name &rest args
                                                         &key (direction nil direction-p)
                                                         &allow-other-keys)
                                  &body body)
    "Evaluate `body` with `stream-name` to an input stream on the file
`file-name`. `args` is sent as is to the call to `open` except `external-format`,
which is only sent to `with-open-file` when it's not `nil`."
    (declare (ignore direction))
    (when direction-p
      (error "Can't specifiy :DIRECTION for WITH-INPUT-FROM-FILE."))
    `(with-open-file* (,stream-name ,file-name :direction :input ,@args)
       ,@body))
  

  (defun read-file-into-string (pathname &key (buffer-size 4096) external-format)
    "Return the contents of the file denoted by `pathname` as a fresh string.

The `external-format` parameter will be passed directly to `with-open-file`
unless it's `nil`, which means the system default."
    (with-input-from-file
        (file-stream pathname :external-format external-format)
      (let ((*print-pretty* nil))
        (with-output-to-string (datum)
          (let ((buffer (make-array buffer-size :element-type 'character)))
            (loop
              :for bytes-read = (read-sequence buffer file-stream)
              :do (write-sequence buffer datum :start 0 :end bytes-read)
              :while (= bytes-read buffer-size)))))))
  

  (declaim (inline remove/swapped-arguments))
  (defun remove/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'remove item sequence keyword-arguments))

  (define-modify-macro removef (item &rest remove-keywords)
    remove/swapped-arguments
    "Modify-macro for `remove`. Sets place designated by the first argument to
the result of calling `remove` with `item`, place, and the `keyword-arguments`.")
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(define-constant deletef read-file-into-string removef)))

;;;; END OF quickutils.lisp ;;;;
