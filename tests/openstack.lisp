(defpackage cl-openstack-client-test
  (:use cl)
  (:export with-function-patch))

(in-package :cl-openstack-client-test)

(defmacro with-function-patch (patch &rest body)
  "Takes a PATCH form like a FLET clause, i.e. (fn-name (lambda-list) body),
evaluates BODY in an environment with fn-name rebound to the PATCH form and
uses UNWIND-PROTECT to safely restore the original definition afterwards."
  (let ((oldfn (gensym))
        (result (gensym))
        (name (car patch))
        (args (cadr patch))
        (pbody (cddr patch)))
    `(let ((,oldfn (symbol-function ',name)))
       (setf (symbol-function ',name) (lambda ,args ,@pbody))
       (unwind-protect (progn ,@body)
         (setf (symbol-function ',name) ,oldfn))
       ,result)))
