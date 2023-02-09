(ql:quickload "cl-utilities")

(in-package :cl-user)

(defun remove-empty (seq)
  (remove-if (cl-utilities:compose #'zerop #'length) seq))

(defmacro git (&rest arguments)
  `(remove-empty
     (mapcar (lambda (x) (string-trim " " x))
             (cl-utilities:split-sequence #\newline (uiop:run-program (list "git" ,@arguments) :input nil :output :string)))))

(defun modified-files ()
  (git "diff" "--name-only"))

(defun skipped-files ()
  (git "ls-files" "-v"))

(defun skip-file (file-path)
  (git "update-index" "--skip-worktree" file-path))

(defun no-skip-file (file-path)
  (git "update-index" "--no-skip-worktree" file-path))
