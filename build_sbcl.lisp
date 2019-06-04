#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; add Sources/ directory to quicklisp local directories
(push #P"/home/fourier/Sources/lisp/" ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :palm-gcontacts)

(save-lisp-and-die "palm-gcontacts" :executable t :toplevel #'palm-gcontacts:main)
