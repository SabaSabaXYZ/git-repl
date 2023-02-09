(ql:quickload "cl-utilities")

(in-package :cl-user)

(defmacro git (&rest arguments)
  `(mapcar (lambda (x) (string-trim " " x))
           (cl-utilities:split-sequence #\newline (uiop:run-program (list "git" ,@arguments) :input nil :output :string))))
