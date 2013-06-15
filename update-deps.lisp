(load "quicklisp.lisp")
(handler-case (quicklisp-quickstart:install :path (user-homedir-pathname))
  (error nil (load "setup")))
(dolist (file '("../requirements.txt" "../test-requirements.txt"))
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          do (ql:quickload line))))
(push
 ;; Send me a patch to make this simpler please.
 (apply 'make-pathname (list :directory (butlast (pathname-directory (user-homedir-pathname)))))
 asdf:*central-registry*)
(load "../run-tests")
