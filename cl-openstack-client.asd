(defsystem cl-openstack-client
  :author "Julien Danjou <julien@danjou.info>"
  :depends-on (#:drakma #:cl-json #:local-time)
  :description "OpenStack client libraries"
  :components
  ((:file "keystone")))
