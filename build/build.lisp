(unless (find-package :ap)
  (ql:quickload '(:antipodes)))

(declaim (optimize (debug 1) (safety 1) (speed 3)))

(let ((*standard-output* (make-broadcast-stream)) ; shut
      (*error-output* (make-broadcast-stream))) ; up
  (asdf:load-system 'antipodes :force t))

(defun main (&rest argv)
  (declare (ignore argv))
  (ap::main))

(sb-ext:save-lisp-and-die "antipodes"
                          :toplevel 'main
                          :save-runtime-options t
                          :executable t)
