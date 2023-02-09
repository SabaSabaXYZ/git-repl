; sbcl --load git-repl.cl

(ql:quickload '("cl-utilities" "str"))

(in-package :cl-user)

(defmacro defun-public (name arglist &body body)
  `(progn
     (export ',name)
     (defun ,name ,arglist ,@body)))

(defmacro clean-working-directory (&body body)
  `(if (null (modified-files))
       ,@body
       (write-line "You have modified files. Please stash or commit your files before proceeding.")))

(defun add-quotes (text)
  (format nil "\"~a\"" text))

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

(defun-public commands ()
  "Lists every public command. Use (describe 'command-name) for documentation."
  (do-external-symbols (sym)
    (format t "~a~%" sym)))

(defun-public show-symbols ()
  "Lists every available symbol. Use (describe 'symbol-name) for documentation."
  (do-symbols (sym)
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
  (mapcar
    (lambda (x) (str:substring 2 t x))
    (remove-if (lambda (x) (not (str:starts-with-p "S " x))) (git "ls-files" "-v"))))

(defun-public skip-file (file-path)
  "Given a filepath, skips the specified file"
  (git "update-index" "--skip-worktree" (add-quotes file-path)))

(defun-public no-skip-file (file-path)
  "Given a filepath, unskips the specified file"
  (git "update-index" "--no-skip-worktree" (add-quotes file-path)))

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

(defun-public apply-stash ()
  "Applies the top stash entry without getting rid of it"
  (git "stash" "apply"))

(defun-public stash-config ()
  "Stashes your current configuration by first unskipping it and then stashing it. This operation fails if you have any modified files."
  (clean-working-directory
    (progn
      (write-line "Unskipping files...")
      (no-skip-all)
      (write-line "Stashing files...")
      (save-stash))))

(defun-public pop-config ()
  "Pops the stash and skips it to store it as your configuration. This operation fails if you have any modified files."
  (clean-working-directory
    (progn
      (write-line "Popping stash...")
      (pop-stash)
      (write-line "Skipping files...")
      (skip-modified))))

(defun-public apply-config ()
  "Applies the stash and skips it to store it as your configuration. This operation fails if you have any modified files."
  (clean-working-directory
    (progn
      (write-line "Applying stash...")
      (apply-stash)
      (write-line "Skipping files...")
      (skip-modified))))

(defun-public delete-branch (branch-name)
  "Deletes the given local branch name"
  (git "branch" "-d" branch-name))

(defun-public delete-merged-branches ()
  "Deletes every local branch covered by \'git branch --merged\'"
  (mapcar #'delete-branch (git "branch" "--merged")))

(defun-public help ()
  "Prints out usage instructions for this REPL"
  (write-line "This is a Common Lisp REPL.")
  (write-line "Run (commands) for a list of commands.")
  (write-line "Use (describe 'command-name) for documentation on a given command.")
  (write-line "Run (help) to see these instructions again.")
  (write-line "Run (exit) to exit gracefully.")
  nil)

(defun main ()
  (progn
    (help)
    (loop
      (progn
        (format t "> ")
        (force-output)
        (print (eval (read)))
        (terpri)
        (force-output)))))

(defvar *installation-file* "C:/bin/git-repl.exe" "The filepath to install to when running (make-install)")

(defun make (executable-name)
  "Compiles the current state into an executable"
  (progn
    (load (compile-file "git-repl.cl"))
    (save-lisp-and-die executable-name :toplevel #'main :executable t)))

(defun make-install ()
  "Compiles the current state into an executable and installs it to *installation-file*"
  (make *installation-file*))
