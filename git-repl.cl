; sbcl --load git-repl.cl

(ql:quickload '("cl-utilities" "str"))

(in-package :cl-user)

(defvar *grep* "findstr")

(defun remove-empty (text)
  (remove-if #'str:emptyp text))

(defun run-command (command)
  (remove-empty
    (mapcar #'str:trim
            (str:lines
              (uiop:run-program command :ignore-error-status t :force-shell t :input nil :output :string)
              :omit-nulls t))))

(defun git (&rest arguments)
  (run-command (str:join " " (cons "git" arguments))))

(defun status ()
  (git "status"))

(defun add-all ()
  (git "add" "."))

(defun commit ()
  (git "commit" "-v"))

(defun modified-files ()
  (git "diff" "--name-only"))

(defun skipped-files ()
  (mapcar (lambda (x) (str:substring 2 t x)) (git "ls-files" "-v" "|" *grep* "^S")))

(defun skip-file (file-path)
  (git "update-index" "--skip-worktree" file-path))

(defun no-skip-file (file-path)
  (git "update-index" "--no-skip-worktree" file-path))

(defun skip-modified ()
  (mapcar #'skip-file (modified-files)))

(defun no-skip-all ()
  (mapcar #'no-skip-file (skipped-files)))

(defun delete-branch (branch-name)
  (git "branch" "-d" branch-name))

(defun delete-merged-branches ()
  (mapcar #'delete-branch (git "branch" "--merged")))

(defun make (executable-name)
  (progn
    (load (compile-file "git-repl.cl"))
    (save-lisp-and-die executable-name :executable t)))
