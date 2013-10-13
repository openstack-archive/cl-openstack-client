(defpackage cl-keystone-client-test
  (:use fiveam
        cl
        cl-openstack-client-test
        cl-keystone-client))

(in-package :cl-keystone-client-test)

(def-suite keystone :description "Test the Openstack Keystone client.")

(in-suite keystone)

(test make-connection
  "Make a connection testing required fields."
  (is-true
   (make-instance 'connection-v2
                  :username "test"
                  :password "test"
                  :url "test"))
  (signals error
    (make-instance 'connection-v2
                   :password "test"
                   :url "test"))
  (signals error
    (make-instance 'connection-v2
                   :username "test"
                   :url "test"))
  (signals error
    (make-instance 'connection-v2
                   :username "test"
                   :password "test")))
