(defsystem cl-openstack-client-test
  :author "Julien Danjou <julien@danjou.info>"
  :depends-on (#:cl-openstack-client
               #:fiveam
               #:trivial-gray-streams
               #:flexi-streams)
  :description "OpenStack client libraries tests"
  :components
  ((:file "keystone"
          :pathname "tests/keystone"
          :depends-on ("openstack"))
   (:file "openstack"
          :pathname "tests/openstack")))
