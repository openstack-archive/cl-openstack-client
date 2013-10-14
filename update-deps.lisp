(load "quicklisp.lisp")
(handler-case (quicklisp-quickstart:install :path (user-homedir-pathname))
  (error nil (load "setup")))
(dolist (file '("../requirements.txt" "../test-requirements.txt"))
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          do (ql:quickload line))))
