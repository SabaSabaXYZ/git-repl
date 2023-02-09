# Git REPL

## Usage

Git REPL provides a Common Lisp environment containing a set of prebuilt Git-related utilities.
It exposes two modes of operation:

* Single command execution mode → Accessed by providing an S-expression as an argument, e.g. `git-repl (write-line \"hello world\")`
* REPL mode → Accessed by executing `git-repl` without any arguments

The single command execution mode allows `git-repl` to work nicely with git aliases and other standard terminal commands.

Meanwhile, the REPL mode provides an environment from which you may explore the REPL and construct powerful commands to manipulate your repository.
By virtue of providing access to Common Lisp commands, you may define your own functions and macros, or override any existing global variables.
You can then compile a new executable with your patched changes using:

    (make :executable-name "path/to/new/executable.exe" :source-file nil)

## Installation

To install Git REPL, install SBCL and Tcl/Tk via your package manager.
On Windows, open Command Line or Powershell with administrator privileges, then run:

    choco install -y sbcl magicsplat-tcl-tk

Next, install Quicklisp using the following commands:

    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval (quicklisp-quickstart:install)

You are now ready to compile Git REPL.

By default, Git REPL is configured to install to `C:\bin\git-repl.exe`.
To proceed with installing to this location, run:

    sbcl --load git-repl.cl --eval (make)

Otherwise, run:

    sbcl --load git-repl.cl
    (make :executable-name "path/to/new/location/git-repl.exe")

Make sure `git-repl.exe` is on your path.
You should now be able to run `git-repl` from any directory.

## Git aliases

Here is a list of global Git aliases (`git config --global -e`) that you may find useful for working with `git-repl`:

    [alias]
        skip = !git-repl \\(interactive-skip\\)
        skip-all = !git-repl \\(skip-modified\\)
        unskip-all = !git-repl \\(no-skip-all\\)
        skip-one = update-index --skip-worktree
        unskip-one = update-index --no-skip-worktree
        skip-list = !git-repl \\(skipped-files\\)
        stash-config = !git-repl \\(stash-config\\)
        pop-config = !git-repl \\(pop-config\\)
        apply-config = !git-repl \\(apply-config\\)
        make-config = !git-repl \\(config-diff \\\"$1\\\"\\) #"
