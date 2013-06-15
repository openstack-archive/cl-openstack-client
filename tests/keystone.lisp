(defpackage cl-keystone-client-test
  (:use fiveam
        cl
        cl-openstack-client-test
        cl-keystone-client))

(in-package :cl-keystone-client-test)

(def-suite keystone :description "My Example Suite")

(in-suite keystone)

(test make-connection
      "Make a connection object"
      (is-true (make-instance 'connection-v2)))
