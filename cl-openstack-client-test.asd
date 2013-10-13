(defsystem cl-openstack-client-test
  :author "Julien Danjou <julien@danjou.info>"
  :depends-on (#:cl-openstack-client
               #:fiveam
               #:cl-ppcre
               #:chunga
               #:drakma
               #:trivial-gray-streams
               #:flexi-streams
               #:local-time)
  :description "OpenStack client libraries tests"
  :components
  ((:file "keystone"
          :pathname "tests/keystone"
          :depends-on ("openstack"))
   (:file "openstack"
          :pathname "tests/openstack")))
