(defsystem cl-openstack-client
  :author "Julien Danjou <julien@danjou.info>"
  :depends-on (#:drakma #:cl-json #:local-time #:alexandria #:uri-template)
  :description "OpenStack client libraries"
  :components
  ((:file "openstack")
   (:file "keystone" :depends-on ("openstack"))))
