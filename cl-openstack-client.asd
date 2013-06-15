(defsystem cl-openstack-client
  :author "Julien Danjou <julien@danjou.info>"
  :depends-on (#:drakma #:cl-json)
  :description "OpenStack client libraries"
  :components
  ((:file "keystone")))
