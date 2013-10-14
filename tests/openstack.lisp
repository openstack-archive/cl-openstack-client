(defpackage cl-openstack-client.test
  (:use cl
        fiveam)
  (:export tests))

(in-package :cl-openstack-client.test)

(def-suite tests
  :description "cl-openstack-client tests")
