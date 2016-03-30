(let ((asdf-file (make-pathname :directory (pathname-directory (user-homedir-pathname))
                                :name "asdf" :type "lisp")))
  (when (probe-file asdf-file)
    (load asdf-file)))

(load (make-pathname :directory (pathname-directory (user-homedir-pathname))
                     :name "quicklisp" :type "lisp"))
(quicklisp-quickstart:install)
(dolist (file '("../requirements.txt" "../test-requirements.txt"))
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          do (ql:quickload line))))
(quit)
