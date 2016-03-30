(let ((asdf-file (make-pathname :directory (pathname-directory (user-homedir-pathname))
                                :name "asdf" :type "lisp")))
  (when (probe-file asdf-file)
    (load asdf-file)))

(load (merge-pathnames "quicklisp/setup.lisp"
                       (user-homedir-pathname)))

(defun terminate (status)
  #+sbcl     (sb-ext:quit      :unix-status status)    ; SBCL
  #+ccl      (   ccl:quit      status)                 ; Clozure CL
  #+clisp    (   ext:quit      status)                 ; GNU CLISP
  #+cmu      (  unix:unix-exit status)                 ; CMUCL
  #+abcl     (   ext:quit      :status status)         ; Armed Bear CL
  #+allegro  (  excl:exit      status :quiet t)        ; Allegro CL
  (cl-user::quit))           ; Many implementations put QUIT in the sandbox CL-USER package.

(require 'cl-openstack-client-test)
(let ((results (5am:run 'cl-openstack-client.test:tests)))
  (5am:explain! results)
  (terminate (if (eq (5am:results-status results) t) 0 1)))
