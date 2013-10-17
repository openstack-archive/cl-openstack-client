(defpackage cl-openstack-client
  (:use cl)
  (:export assoc*))


(in-package :cl-openstack-client)

(defun assoc* (item alist &rest rest &key key test test-not)
  "Return the CDR of the ASSOC result."
  (declare (ignore key test test-not))
  (cdr (apply #'assoc item alist rest)))
