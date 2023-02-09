; sbcl --load git-repl.cl

(ql:quickload '("cl-utilities" "str"))

(in-package :cl-user)

(defmacro defvar-public (name value &optional doc)
  `(progn
     (export ',name)
     (defvar ,name ,value ,doc)))

(defvar-public *file-chunk-length* 10 "The number of files to operate on at a time for update-index")
(defvar-public *installation-file* "C:/bin/git-repl.exe" "The filepath to install to when running (make-install)")

(defmacro defun-public (name arglist &body body)
  `(progn
     (export ',name)
     (defun ,name ,arglist ,@body)))

(defmacro clean-working-directory (&body body)
  `(if (null (modified-files))
       ,@body
       (write-line "You have modified files. Please stash or commit your files before proceeding.")))

(defmacro config-do (&body body)
  "Unskips your configuration, executes the provided body, then skips your configuration"
  `(clean-working-directory
     (progn
       (no-skip-all)
       ,@body
       (skip-modified))))

(defun add-quotes (text)
  (format nil "\"~a\"" text))

(defun remove-empty (text-lines)
  "Removes every empty line from a list of lines of text"
  (remove-if #'str:emptyp text-lines))

(defun run-command (command &key (list-output t))
  (let ((output (uiop:run-program command :ignore-error-status t :force-shell t :input nil :output :string)))
    (if list-output
        (remove-empty (mapcar #'str:trim (str:lines output :omit-nulls t)))
        output)))

(defun-public run-git (arguments &key (list-output t))
  "Runs an arbitrary git command. Provide the list of arguments as a list of strings, for example: (run-git '(\"add\" \"-i\"))
  When :list-output is set to T (default), the output is provided as a list of strings.
  When :list-output is set to NIL, the raw output is provided as a single string.
  To use :list-output, provide it after the argument as follows: (run-git '(\"add\" \"-i\") :list-output nil)"
  (run-command (str:join " " (cons "git" arguments)) :list-output list-output))

(defun-public git (&rest arguments)
  "Runs an arbitrary git command. Separate each token as a string, for example: (git \"add\" \"-i\")"
  (run-git arguments))

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

(defun chunk-list (len arglist)
  (loop
    for front = arglist then next
    for next = (nthcdr len front)
    collect (ldiff front next)
    while next))

(defun-public handle-file-paths (arg)
  "Accepts either a string or a list of strings. Returns a list of strings, each one surrounded by \", chunked in lists of length *file-chunk-length*"
  (typecase arg
    (string (list (add-quotes arg)))
    (list (mapcar (lambda (x) (str:join " " x)) (chunk-list *file-chunk-length* (mapcar #'add-quotes arg))))
    (t (error "Argument must be either a string or a list of strings."))))

(defun-public git-chunked-command (command subcommand file-path)
  "Runs the specified command on the filepath or list of filepaths"
  (mapcar (lambda (x) (git command subcommand "--" x)) (handle-file-paths file-path)))

(defun-public update-index (command file-path)
  "Runs the specified update-index command on the filepath or list of filepaths"
  (git-chunked-command "update-index" command file-path))

(defun-public skip-file (file-path)
  "Given a filepath or list of filepaths, skips the specified files"
  (update-index "--skip-worktree" file-path))

(defun-public no-skip-file (file-path)
  "Given a filepath or list of filepaths, unskips the specified files"
  (update-index "--no-skip-worktree" file-path))

(defun-public skip-modified ()
  "Skips every modified file"
  (skip-file (modified-files)))

(defun-public no-skip-all ()
  "Unskips every skipped file"
  (no-skip-file (skipped-files)))

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

(defun-public config-diff (file-path)
  "Saves the current configuration to the specified file"
  (config-do (str:to-file file-path (run-git '("diff") :list-output nil))))

(defun-public delete-branch (branch-name)
  "Deletes the given local branch name"
  (git "branch" "-d" branch-name))

(defun-public delete-merged-branches ()
  "Deletes every local branch covered by \'git branch --merged\'"
  (git-chunked-command "branch" "-d" (git "branch" "--merged")))

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

(defun-public make (executable-name)
  "Compiles the current state into an executable"
  (progn
    (load (compile-file "git-repl.cl"))
    (save-lisp-and-die executable-name :toplevel #'main :executable t)))

(defun-public make-install ()
  "Compiles the current state into an executable and installs it to *installation-file*"
  (make *installation-file*))
