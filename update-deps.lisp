(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))

(dolist (file '("../requirements.txt" "../test-requirements.txt"))
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          do (ql:quickload line))))
(quit)
