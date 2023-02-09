(load (compile-file "git-skipper.cl"))
(save-lisp-and-die "git-skipper.exe" :executable t)

; sbcl --load git-skipper.cl
