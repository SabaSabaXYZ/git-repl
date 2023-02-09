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

(defun modified-files ()
  (git "diff" "--name-only"))

(defun skipped-files ()
  (mapcar (lambda (x) (str:substring 2 t x)) (git "ls-files" "-v" "|" *grep* "^S")))

(defun skip-file (file-path)
  (git "update-index" "--skip-worktree" file-path))

(defun no-skip-file (file-path)
  (git "update-index" "--no-skip-worktree" file-path))

(defun status ()
  (git "status"))

(no-skip-file "git-skipper.cl")
(skip-file "git-skipper.cl")
(modified-files)
(skipped-files)
