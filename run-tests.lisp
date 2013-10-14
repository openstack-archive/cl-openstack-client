;; ;; Add the 
;; (push
;;  ;; Send me a patch to make this simpler please.
;;  (apply 'make-pathname (list :directory (butlast (pathname-directory (user-homedir-pathname)))))
;;  asdf:*central-registry*)

(defun terminate (status)
  #+sbcl     (sb-ext:quit      :unix-status status)    ; SBCL
  #+ccl      (   ccl:quit      status)                 ; Clozure CL
  #+clisp    (   ext:quit      status)                 ; GNU CLISP
  #+cmu      (  unix:unix-exit status)                 ; CMUCL
  #+abcl     (   ext:quit      :status status)         ; Armed Bear CL
  #+allegro  (  excl:exit      status :quiet t)        ; Allegro CL
  (cl-user::quit))           ; Many implementations put QUIT in the sandbox CL-USER package.

(require 'cl-openstack-client-test)
(let ((test-to-run 'kawoosh.test:kawoosh.test))
(let ((results (5am:run test-to-run)))
  (5am:explain! results)
  (terminate (if (eq (5am:results-status results) t) 0 1))))
