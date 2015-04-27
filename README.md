cl-openstack-client
===================

Common Lisp OpenStack client libraries


To get an initial keystone connection:

    (setf keystone-connection
        (make-instance 'cl-keystone-client:connection-v2
            :username "openstack-user"
            :tenant-name "my tenant"
            :password "secret"
            :url "https://cloud.aptira.com:5000/"))


Authenticate the connection:

    (cl-keystone-client:authenticate keystone-connection)
