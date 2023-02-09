; sbcl --load git-repl.cl

(ql:quickload '("cl-utilities" "str"))

(in-package :cl-user)

(defvar *grep* "findstr" "The name of the program used for searching. On Windows this is set to \'findstr\', but it may be changed to \'grep\' for Unix systems.")

(defmacro defun-public (name arglist &body body)
  `(progn
     (export ',name)
     (defun ,name ,arglist ,@body)))

(defmacro clean-working-directory (&body body)
  `(if (null (modified-files))
       (write-line "You have modified files. Please stash or commit your files before proceeding.")
       ,@body))

(defun remove-empty (text-lines)
  "Removes every empty line from a list of lines of text"
  (remove-if #'str:emptyp text-lines))

(defun run-command (command)
  (remove-empty
    (mapcar #'str:trim
            (str:lines
              (uiop:run-program command :ignore-error-status t :force-shell t :input nil :output :string)
              :omit-nulls t))))

(defun-public git (&rest arguments)
  (run-command (str:join " " (cons "git" arguments))))

(defun-public help ()
  (do-external-symbols (sym)
    (format t "~a~%" sym)))

(defun-public status ()
  (git "status"))

(defun-public add-all ()
  (git "add" "."))

(defun-public commit ()
  (git "commit" "-v"))

(defun-public modified-files ()
  (git "diff" "--name-only"))

(defun-public skipped-files ()
  (mapcar (lambda (x) (str:substring 2 t x)) (git "ls-files" "-v" "|" *grep* "^S")))

(defun-public skip-file (file-path)
  (git "update-index" "--skip-worktree" file-path))

(defun-public no-skip-file (file-path)
  (git "update-index" "--no-skip-worktree" file-path))

(defun-public skip-modified ()
  (mapcar #'skip-file (modified-files)))

(defun-public no-skip-all ()
  (mapcar #'no-skip-file (skipped-files)))

(defun-public save-stash ()
  (git "stash"))

(defun-public pop-stash ()
  (git "stash" "pop"))

(defun-public stash-config ()
  (clean-working-directory
    (progn
      (no-skip-all)
      (save-stash))))

(defun-public pop-config ()
  "Pops the stash and skips it to store it as your configuration"
  (clean-working-directory
    (progn
      (pop-stash)
      (skip-modified))))

(defun-public delete-branch (branch-name)
  "Deletes the given local branch name"
  (git "branch" "-d" branch-name))

(defun-public delete-merged-branches ()
  "Deletes every local branch covered by \'git branch --merged\'"
  (mapcar #'delete-branch (git "branch" "--merged")))

(defun-public make (executable-name)
  "Compiles the current state into an executable"
  (progn
    (load (compile-file "git-repl.cl"))
    (save-lisp-and-die executable-name :executable t)))
