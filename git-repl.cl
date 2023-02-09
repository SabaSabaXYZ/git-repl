; sbcl --load git-repl.cl

(ql:quickload '("cl-utilities" "str"))

(in-package :cl-user)

(defmacro defvar-public (name value &optional doc)
  `(progn
     (export ',name)
     (defvar ,name ,value ,doc)))

(defmacro defun-public (name arglist &body body)
  `(progn
     (export ',name)
     (defun ,name ,arglist ,@body)))

(defmacro clean-working-directory (&body body)
  `(if (null (modified-files))
       (write-line "You have modified files. Please stash or commit your files before proceeding.")
       ,@body))

(defvar-public *grep* "findstr" "The name of the program used for searching. On Windows this is set to \'findstr\', but it may be changed to \'grep\' for Unix systems.")

(defvar-public *installation-file* "C:/bin/git-repl.exe" "The filepath to install to when running (make-install)")

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
  "Runs an arbitrary git command. Separate each token as a string, for example: (git \"add\" \"-i\")"
  (run-command (str:join " " (cons "git" arguments))))

(defun-public help ()
  "Lists every public command. Use (describe 'command-name) for documentation."
  (do-external-symbols (sym)
    (format t "~a~%" sym)))

(defun-public status ()
  "Displays the current status of the repository"
  (git "status"))

(defun-public add-all ()
  "Adds every file under the current directory. Executes \'git add .\'"
  (git "add" "."))

(defun-public commit ()
  "Performs a verbose commit"
  (git "commit" "-v"))

(defun-public modified-files ()
  "Displays a list of modified, unstaged files"
  (git "diff" "--name-only"))

(defun-public skipped-files ()
  "Displays a list of skipped files"
  (mapcar (lambda (x) (str:substring 2 t x)) (git "ls-files" "-v" "|" *grep* "^S")))

(defun-public skip-file (file-path)
  "Given a filepath, skips the specified file"
  (git "update-index" "--skip-worktree" file-path))

(defun-public no-skip-file (file-path)
  "Given a filepath, unskips the specified file"
  (git "update-index" "--no-skip-worktree" file-path))

(defun-public skip-modified ()
  "Skips every modified file"
  (mapcar #'skip-file (modified-files)))

(defun-public no-skip-all ()
  "Unskips every skipped file"
  (mapcar #'no-skip-file (skipped-files)))

(defun-public save-stash ()
  "Stashes every modified file"
  (git "stash"))

(defun-public pop-stash ()
  "Pops the top stash entry"
  (git "stash" "pop"))

(defun-public stash-config ()
  "Stashes your current configuration by first unskipping it and then stashing it. This operation fails if you have any modified files."
  (clean-working-directory
    (progn
      (no-skip-all)
      (save-stash))))

(defun-public pop-config ()
  "Pops the stash and skips it to store it as your configuration. This operation fails if you have any modified files."
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

(defun-public make-install ()
  "Compiles the current state into an executable and installs it to *installation-file*"
  (make *installation-file*))
