; sbcl --load git-repl.cl --eval "(make)"

(ql:quickload '("str" "ltk"))

(in-package :cl-user)

(defmacro defvar-public (name value &optional doc)
  `(progn
     (export ',name)
     (defvar ,name ,value ,doc)))

(defvar-public *file-chunk-length* 10 "The number of files to operate on at a time for handle-file-paths")
(defvar-public *installation-file* "C:/bin/git-repl.exe" "The filepath to install to when running (make)")
(defvar-public *quiet-mode* nil "When set to T, disables any non-essential logging when executing a command")

(defmacro print-or-collect-symbols (do-symbol-action &key (print-symbols t))
  (let ((sym (gensym)) (results (gensym)))
    `(let ((,results nil))
       (progn
         (,do-symbol-action (,sym)
                            (push (format ,print-symbols (if ,print-symbols "~a~%" "~a") ,sym) ,results))
         (if ,print-symbols nil ,results)))))

(defmacro defun-public (name arglist &body body)
  `(progn
     (export ',name)
     (defun ,name ,arglist ,@body)))

(defmacro log-action (description &body body)
  `(progn
     (if (not *quiet-mode*)
         (write-line ,description)
         nil)
     ,@body))

(defmacro clean-working-directory (&body body)
  `(if (null (modified-files))
       (progn ,@body)
       (write-line "You have modified files. Please stash or commit your files before proceeding.")))

(defmacro config-do (&body body)
  "Unskips your configuration, executes the provided body, then skips your configuration"
  `(clean-working-directory
     (no-skip-all)
     ,@body
     (skip-modified)))

(defmacro without-config-do (&body body)
  "Stashes your configuration, executes the provided body, then restores your configuration"
  `(clean-working-directory
     (stash-config)
     ,@body
     (pop-config)))

(defmacro toggle-without-config (without-config &body body)
  "Handles the common pattern of toggling whether to execute a command within the context of without-config-do or not"
  `(if ,without-config
       (without-config-do ,@body)
       (progn ,@body)))

(defun add-quotes (text)
  (format nil "\"~a\"" text))

(defun run-command (command &key (list-output nil))
  (let ((output (uiop:run-program command :ignore-error-status t :force-shell t :input nil :output :string :error-output :output)))
    (if list-output
        (remove-if #'str:emptyp (mapcar #'str:trim (str:lines output :omit-nulls t)))
        output)))

(defun-public run-git (arguments &key (list-output nil))
  "Runs an arbitrary git command. Provide the list of arguments as a list of strings, for example: (run-git '(\"add\" \".\"))
  When :list-output is set to NIL (default), the raw output is provided as a single string.
  When :list-output is set to T, the output is provided as a list of strings.
  To use :list-output, provide it after the argument as follows: (run-git '(\"add\" \".\") :list-output nil)"
  (run-command (str:join " " (cons "git" arguments)) :list-output list-output))

(defun-public git (&rest arguments)
  "Runs an arbitrary git command. Separate each token as a string, for example: (git \"add\" \".\")"
  (run-git arguments :list-output t))

(defun-public commands (&key (print-symbols t))
  "Lists every public symbol. Use (describe 'command-name) for documentation.
  If :print-symbols is T (default), print every public symbol to standard output.
  Otherwise, returns each symbol in a list."
  (print-or-collect-symbols do-external-symbols :print-symbols print-symbols))

(defun-public show-symbols (&key (print-symbols t))
  "Lists every available symbol. Use (describe 'symbol-name) for documentation.
  If :print-symbols is T (default), print every symbol to standard output.
  Otherwise, returns each symbol in a list."
  (print-or-collect-symbols do-symbols :print-symbols print-symbols))

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

(defun handle-skip-file-pattern (skip-action files-action pattern predicate)
  (funcall skip-action (remove-if-not (lambda (file-name) (funcall predicate pattern file-name)) (funcall files-action))))

(defun-public skip-file-pattern (pattern &key (predicate #'str:ends-with-p))
  "Skips every file with the specified pattern in the filename.
  By default, the pattern is checked only at the end of the filename.
  Use STR:STARTS-WITH-P or STR:CONTAINSP for different behaviour."
  (handle-skip-file-pattern #'skip-file #'modified-files pattern predicate))

(defun-public no-skip-file-pattern (pattern &key (predicate #'str:ends-with-p))
  "Unskips every file with the specified pattern in the filename.
  By default, the pattern is checked only at the end of the filename.
  Use STR:STARTS-WITH-P or STR:CONTAINSP for different behaviour."
  (handle-skip-file-pattern #'no-skip-file #'skipped-files pattern predicate))

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
    (log-action "Unskipping files..." (no-skip-all))
    (log-action "Stashing files..." (save-stash))))

(defun-public pop-config ()
  "Pops the stash and skips it to store it as your configuration. This operation fails if you have any modified files."
  (clean-working-directory
    (log-action "Popping stash..." (pop-stash))
    (log-action "Skipping files..." (skip-modified))))

(defun-public apply-config ()
  "Applies the stash and skips it to store it as your configuration. This operation fails if you have any modified files."
  (clean-working-directory
    (log-action "Applying stash..." (apply-stash))
    (log-action "Skipping files..." (skip-modified))))

(defun-public config-diff (file-path)
  "Saves the current configuration to the specified file"
  (config-do (str:to-file file-path (run-git '("diff")))))

(defun-public set-config (file-path)
  "Applies the configuration from the specified file and skips it"
  (clean-working-directory
    (git "apply" file-path)
    (skip-modified)))

(defun-public delete-branch (branch-name)
  "Deletes the given local branch name"
  (git "branch" "-d" branch-name))

(defun delete-branches (is-merged force-delete)
  "Deletes every local branch covered by \'git branch --merged\' or \'git branch --no-merged\' based on the value of the argument \'is-merged\'.
  When force-delete is true, forces the deletion of the branch even if it has unmerged changes."
  (git-chunked-command "branch" (if force-delete "-D" "-d") (git "branch" (if is-merged "--merged" "--no-merged"))))

(defun-public delete-merged-branches (&key (force-delete nil))
  "Deletes every local branch covered by \'git branch --merged\'.
  When :force-delete is set to T (default is NIL), forces the deletion of the branch even if it has unmerged changes."
  (delete-branches t force-delete))

(defun-public delete-no-merged-branches (&key (force-delete nil))
  "Deletes every local branch covered by \'git branch --no-merged\'.
  When :force-delete is set to T (default is NIL), forces the deletion of the branch even if it has unmerged changes."
  (delete-branches nil force-delete))

(defun-public rebase (revision &key (without-config nil))
  "Performs a rebase
  If :without-config is set to T (default is NIL), stashes your configuration before the rebase and pops the stash afterwards."
  (toggle-without-config without-config (git "rebase" revision)))

(defun-public checkout (revision &key (without-config nil))
  "Checks out the specified revision.
  If :without-config is set to T (default is NIL), stashes your configuration before the checkout and pops the stash afterwards."
  (toggle-without-config without-config (git "checkout" revision)))

(defun-public reset-branch (revision &key (mode 'mixed) (without-config nil))
  "Performs a reset to the specified revision.
  The :mode must be either 'soft, 'mixed (default), or 'hard.
  If :without-config is set to T (default is NIL), stashes your configuration before the checkout and pops the stash afterwards."
  (let ((mode-string (cond
                       ((eq mode 'soft) "--soft")
                       ((eq mode 'mixed) "--mixed")
                       ((eq mode 'hard) "--hard")
                       (t (error "Mode must be either 'soft, 'mixed, or 'hard")))))
    (toggle-without-config without-config (git "reset" mode-string revision))))

(defun-public mergetool ()
  "Executes the mergetool configured with git"
  (git "mergetool"))

(defparameter *skipped-files* (skipped-files) "Stores the currently skipped files for use by interactive-skip")
(defparameter *modified-files* (modified-files) "Stores the currently modified files for use by interactive-skip")

(defun handle-interactive-skip (action refresh-action listbox item-type)
  (lambda ()
    (progn
      (let* ((items (if (eq item-type 'skipped) *skipped-files* *modified-files*))
             (selected-items (mapcar (lambda (x) (if x (nth x items) nil)) (ltk:listbox-get-selection listbox))))
        (funcall action selected-items))
      (funcall refresh-action))))

(defun refresh-interactive-skip (skipped-file-list modified-file-list)
  (lambda ()
    (ltk:listbox-clear (ltk:listbox skipped-file-list))
    (ltk:listbox-clear (ltk:listbox modified-file-list))
    (setf *skipped-files* (skipped-files))
    (setf *modified-files* (modified-files))
    (ltk:listbox-append skipped-file-list *skipped-files*)
    (ltk:listbox-append modified-file-list *modified-files*)))

(defun-public interactive-skip ()
  "Displays a window that lets you skip and unskip files interactively"
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* "Interactive Skipper")
    (let* ((content (make-instance 'ltk:frame))
           (skipped-content (make-instance 'ltk:frame :master content :borderwidth 2 :relief 'groove))
           (modified-content (make-instance 'ltk:frame :master content :borderwidth 2 :relief 'groove))
           (button-content (make-instance 'ltk:frame :master content :borderwidth 2 :relief 'groove))
           (skipped-files-label (make-instance 'ltk:label :text "Skipped Files" :master skipped-content))
           (modified-files-label (make-instance 'ltk:label :text "Modified Files" :master modified-content))
           (skipped-file-list (make-instance 'ltk:scrolled-listbox :master skipped-content))
           (modified-file-list (make-instance 'ltk:scrolled-listbox :master modified-content))
           (refresh-action (refresh-interactive-skip skipped-file-list modified-file-list))
           (skip-button (make-instance 'ltk:button :text "← Skip" :master button-content :command (handle-interactive-skip #'skip-file refresh-action modified-file-list 'modified)))
           (no-skip-button (make-instance 'ltk:button :text "Unskip →" :master button-content :command (handle-interactive-skip #'no-skip-file refresh-action skipped-file-list 'skipped)))
           (refresh-button (make-instance 'ltk:button :text "Refresh" :command refresh-action :master button-content)))
      (ltk:configure (ltk:listbox skipped-file-list) :selectmode 'multiple)
      (ltk:configure (ltk:listbox modified-file-list) :selectmode 'multiple)
      (funcall refresh-action)

      (ltk:configure content :padding "5 5 5 5")
      (ltk:grid-columnconfigure ltk:*tk* 0 :weight 1)
      (ltk:grid-rowconfigure ltk:*tk* 0 :weight 1)

      (ltk:grid-columnconfigure content 0 :weight 100)
      (ltk:grid-columnconfigure content 1 :weight 1)
      (ltk:grid-columnconfigure content 2 :weight 100)
      (ltk:grid-rowconfigure content 0 :weight 1)

      (ltk:grid-columnconfigure skipped-content 0 :weight 1)
      (ltk:grid-rowconfigure skipped-content 0 :weight 1)
      (ltk:grid-rowconfigure skipped-content 1 :weight 100)

      (ltk:grid-columnconfigure modified-content 0 :weight 1)
      (ltk:grid-rowconfigure modified-content 0 :weight 1)
      (ltk:grid-rowconfigure modified-content 1 :weight 100)

      (ltk:grid-columnconfigure button-content 0 :weight 1)
      (ltk:grid-rowconfigure button-content 0 :weight 1)
      (ltk:grid-rowconfigure button-content 1 :weight 1)
      (ltk:grid-rowconfigure button-content 2 :weight 1)

      (ltk:grid content 0 0 :sticky "nsew")
      (ltk:grid skipped-content 0 0 :sticky "nsew")
      (ltk:grid button-content 0 1 :sticky "nsew")
      (ltk:grid modified-content 0 2 :sticky "nsew")
      (ltk:grid skipped-files-label 0 0)
      (ltk:grid modified-files-label 0 0)
      (ltk:grid skipped-file-list 1 0 :sticky "nsew")
      (ltk:grid modified-file-list 1 0 :sticky "nsew")
      (ltk:grid skip-button 0 0)
      (ltk:grid refresh-button 1 0)
      (ltk:grid no-skip-button 2 0))))

(defun-public help ()
  "Prints out usage instructions for this REPL"
  (write-line "This is a Common Lisp REPL.")
  (write-line "Run (commands) for a list of commands.")
  (write-line "Use (describe 'command-name) for documentation on a given command.")
  (write-line "Run (help) to see these instructions again.")
  (write-line "Run (exit) to exit gracefully.")
  nil)

(defun repl ()
  (loop
    (progn
      (format t "> ")
      (force-output)
      (print (eval (read)))
      (terpri)
      (force-output))))

(defun execute-single-command (lisp-command)
  (let ((result (eval (read-from-string lisp-command))))
    (if *quiet-mode*
        nil
        (print result))))

(defun process-args (initial-args)
  (if (equal "-q" (car initial-args))
      (progn
        (setf *quiet-mode* t)
        (cdr initial-args))
      initial-args))

(defun main ()
  (let ((args (process-args (cdr *posix-argv*))))
    (if args
        (execute-single-command (str:join " " args))
        (progn
          (help)
          (repl)))))

(defun-public make (&key (executable-name *installation-file*) (source-file "git-repl.cl"))
  "Compiles the current state into an executable"
  (progn
    (if source-file
        (load (compile-file source-file))
        nil)
    (save-lisp-and-die executable-name :toplevel #'main :executable t)))
