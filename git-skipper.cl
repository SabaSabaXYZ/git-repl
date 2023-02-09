(ql:quickload '("cl-utilities" "str"))

(in-package :cl-user)

(defun remove-empty (seq)
  (remove-if #'str:emptyp seq))

(defmacro git (input &rest arguments)
  `(remove-empty
     (mapcar #'str:trim (str:lines (uiop:run-program (list "git" ,@arguments) :force-shell t :input ,input :output :string) :omit-nulls t))))

(defun modified-files ()
  (git nil "diff" "--name-only"))

(defun skipped-files ()
  (git nil "ls-files" "-v" "|" "findstr" "^S"))

(defun skip-file (file-path)
  (git nil "update-index" "--skip-worktree" file-path))

(defun no-skip-file (file-path)
  (git nil "update-index" "--no-skip-worktree" file-path))

(defun status ()
  (git nil "status"))

(no-skip-file "git-skipper.cl")
(skip-file "git-skipper.cl")
(modified-files)
(skipped-files)
(status)
(git nil "commit" "-av")
