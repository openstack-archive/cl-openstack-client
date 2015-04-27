(defpackage cl-keystone-client.test
  (:use fiveam
        cl
        cl-keystone-client)
  (:import-from #:drakma
                #:header-value)
  (:import-from #:cl-openstack-client.test
                #:connection-fixture
                #:user-fixture
                #:tenant-fixture
                #:with-mock-http-stream
                #:make-mock-http-stream
                #:mock-response
                #:read-mock-request
                #:mock-http-stream
                #:is-valid-request)
  (:import-from #:local-time
                #:encode-timestamp
                #:timestamp-to-unix
                #:timestamp=
                #:timestamp+
                #:format-timestring
                #:now
                #:+utc-zone+))

(in-package :cl-keystone-client.test)

(def-suite tests
  :in cl-openstack-client.test:tests
  :description "Test the Openstack Keystone client.")

(in-suite tests)

(defparameter +keystone-format+
  ;; same as +ISO-8601-FORMAT+ except with non nano seconds.
  '((:year 4) #\- (:month 2) #\- (:day 2) #\T
    (:hour 2) #\: (:min 2) #\: (:sec 2)
    :gmt-offset-or-z))

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

(test token-expiry-conversion
  "Test that the token expiry is correctly converted to a local-time
object."
  (let ((connection (connection-fixture)))
    (setf (slot-value connection 'cl-keystone-client::token)
          '((:issued-at . "2013-10-13T06:01:36.315343")
            (:expires . "2013-10-14T06:01:36Z")))
    (is (timestamp=
         (connection-token-expires connection)
         (encode-timestamp 0 36 1 6 14 10 2013
                           :timezone +utc-zone+)))
    (is (timestamp=
         (connection-token-issued-at connection)
         (encode-timestamp 315343000 36 1 6 13 10 2013
                           :timezone +utc-zone+)))))

(test token-expired
  "Test that the token expiry is detected correctly."
  (let ((connection (connection-fixture)))
    (setf (slot-value connection 'cl-keystone-client::token)
          '((:expires . "2013-10-12T06:01:36Z")))
    (is-false (connection-token-valid-p connection))))

(test token-valid
  "Test the validity of a token is detected correctly."
  (let ((connection (connection-fixture)))
    (setf (slot-value connection 'cl-keystone-client::token)
          `((:expires . ,(format-timestring
                          nil
                          (timestamp+ (now) 1 :minute)
                          :format +keystone-format+
                          :timezone +utc-zone+))))
    (is-true (connection-token-valid-p connection))))

(test authentication-error-404
  "Test that the correct condition is signalled when a 404 is returned
from the keystone server."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   404
                   :content "{\"error\": {\"message\": \"The resource could not be found.\", \"code\": 404, \"title\": \"Not Found\"}}")
    (handler-case
        (authenticate (make-instance 'connection-v2
                                     :tenant-name "test"
                                     :url "http://test:33"
                                     :username "test"
                                     :password "test"))
      (keystone-error (keystone-error)
        (is (eql (error-code keystone-error)
                 404))))
    (destructuring-bind (status headers content)
        (read-mock-request mock-stream)
      (is (equal content
                 "{\"auth\":{\"passwordCredentials\":{\"username\":\"test\",\"password\":\"test\"},\"tenantName\":\"test\"}}"))
      (is (string-equal "application/json"
                        (header-value :content-type headers)))
      (is (string-equal "test:33"
                        (header-value :host headers)))
      (is (eql (getf status :method) :post))
      (is (string-equal (getf status :uri) "/v2.0/tokens")))))


(test authentication
  "Test that authentication correctly initialises the connection object."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"access\": {\"token\": {\"issued_at\": \"2013-10-28T21:31:34.158770\", \"expires\": \"2013-10-29T21:31:34Z\", \"id\": \"MIINUAYJKoZIhvcNAQ==\", \"tenant\": {\"description\": null, \"enabled\": true, \"id\": \"36215f8\", \"name\": \"admin\"}}, \"serviceCatalog\": [{\"endpoints\": [{\"adminURL\": \"http://192.168.1.9:8774/v2/36215f8\", \"region\": \"RegionOne\", \"internalURL\": \"http://192.168.1.9:8774/v2/36215f8\", \"id\": \"53ad66f\", \"publicURL\": \"http://192.168.1.9:8774/v2/36215f8\"}], \"endpoints_links\": [], \"type\": \"compute\", \"name\": \"nova\"}, {\"endpoints\": [{\"adminURL\": \"http://192.168.1.9:35357/v2.0\", \"region\": \"RegionOne\", \"internalURL\": \"http://192.168.1.9:5000/v2.0\", \"id\": \"1d6a58b\", \"publicURL\": \"http://192.168.1.9:5000/v2.0\"}], \"endpoints_links\": [], \"type\": \"identity\", \"name\": \"keystone\"}], \"user\": {\"username\": \"admin\", \"roles_links\": [], \"id\": \"717a936\", \"roles\": [{\"name\": \"admin\"}], \"name\": \"admin\"}, \"metadata\": {\"is_admin\": 0, \"roles\": [\"a0dfe95\"]}}}")
    (let ((connection
            (authenticate (make-instance 'connection-v2
                                         :tenant-name "test"
                                         :url "http://test:5000"
                                         :username "test"
                                         :password "test"))))
      (is (equal (connection-token-id connection)
                 "MIINUAYJKoZIhvcNAQ=="))
      (is (equal (connection-service-catalog connection)
                 '(((:endpoints
                     ((:admin-url . "http://192.168.1.9:8774/v2/36215f8")
                      (:region . "RegionOne")
                      (:internal-url . "http://192.168.1.9:8774/v2/36215f8")
                      (:id . "53ad66f")
                      (:public-url . "http://192.168.1.9:8774/v2/36215f8")))
                    (:endpoints-links)
                    (:type . "compute")
                    (:name . "nova"))
                   ((:endpoints
                     ((:admin-url . "http://192.168.1.9:35357/v2.0")
                      (:region . "RegionOne")
                      (:internal-url . "http://192.168.1.9:5000/v2.0")
                      (:id . "1d6a58b")
                      (:public-url . "http://192.168.1.9:5000/v2.0")))
                    (:endpoints-links)
                    (:type . "identity")
                    (:name . "keystone")))))
      (with-slots (id name enabled description)
          (connection-tenant connection)
        (is (equal (list id name enabled description)
                   (list "36215f8" "admin" t nil))))
      (is (timestamp=
         (connection-token-expires connection)
         (encode-timestamp 0 34 31 21 29 10 2013
                           :timezone +utc-zone+)))
      (is (timestamp=
         (connection-token-issued-at connection)
         (encode-timestamp 158770000 34 31 21 28 10 2013
                           :timezone +utc-zone+))))
    (destructuring-bind (status headers content)
        (read-mock-request mock-stream)
      (is (equal content
                 "{\"auth\":{\"passwordCredentials\":{\"username\":\"test\",\"password\":\"test\"},\"tenantName\":\"test\"}}"))
      (is (string-equal "application/json"
                        (header-value :content-type headers)))
      (is (string-equal "test:5000"
                        (header-value :host headers)))
      (is (eql (getf status :method) :post))
      (is (string-equal (getf status :uri) "/v2.0/tokens")))))


(test api-versions
  "Test listing the API version details"
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"versions\": {\"values\": [{\"status\": \"stable\", \"updated\": \"2013-03-06T00:00:00Z\", \"media-types\": [{\"base\": \"application/json\", \"type\": \"application/vnd.openstack.identity-v3+json\"}, {\"base\": \"application/xml\", \"type\": \"application/vnd.openstack.identity-v3+xml\"}], \"id\": \"v3.0\", \"links\": [{\"href\": \"http://192.168.122.210:5000/v3/\", \"rel\": \"self\"}]}, {\"status\": \"stable\", \"updated\": \"2014-04-17T00:00:00Z\", \"media-types\": [{\"base\": \"application/json\", \"type\": \"application/vnd.openstack.identity-v2.0+json\"}, {\"base\": \"application/xml\", \"type\": \"application/vnd.openstack.identity-v2.0+xml\"}], \"id\": \"v2.0\", \"links\": [{\"href\": \"http://192.168.122.210:5000/v2.0/\", \"rel\": \"self\"}, {\"href\": \"http://docs.openstack.org/\", \"type\": \"text/html\", \"rel\": \"describedby\"}]}]}}")
    (let ((version-response (list-versions (connection-fixture))))
      (destructuring-bind (status headers content)
          (read-mock-request mock-stream)
        (is (eql (getf status :method) :get))
        (is (string-equal (getf status :uri) "/")))
      (is (equal version-response
                 '((:versions
                    (:values
                     ((:status . "stable") (:updated . "2013-03-06T00:00:00Z")
                      (:media-types
                       ((:base . "application/json")
                        (:type . "application/vnd.openstack.identity-v3+json"))
                       ((:base . "application/xml")
                        (:type . "application/vnd.openstack.identity-v3+xml")))
                      (:id . "v3.0")
                      (:links ((:href . "http://192.168.122.210:5000/v3/") (:rel . "self"))))
                     ((:status . "stable") (:updated . "2014-04-17T00:00:00Z")
                      (:media-types
                       ((:base . "application/json")
                        (:type . "application/vnd.openstack.identity-v2.0+json"))
                       ((:base . "application/xml")
                        (:type . "application/vnd.openstack.identity-v2.0+xml")))
                      (:id . "v2.0")
                      (:links ((:href . "http://192.168.122.210:5000/v2.0/") (:rel . "self"))
                       ((:href . "http://docs.openstack.org/") (:type . "text/html")
                        (:rel . "describedby"))))))))))))

;;
;; Tenants
;;

(test add-tenant
  "Test the adding a tenant."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"tenant\": {\"description\": \"test description\", \"enabled\": true, \"id\": \"52dadc45c0c14e519f5026dbe5259fbc\", \"name\": \"test123\"}}}")
    (let ((tenant (add-tenant (connection-fixture)
                             :name "test123" :description "test description")))
      (is-valid-request mock-stream :post "/v2.0//tenants"
                        :content
                        "{\"tenant\":{\"name\":\"test123\",\"description\":\"test description\",\"enabled\":true}}"
                        :host "192.168.1.9:35357")
      (is (equal (tenant-name tenant)
                 "test123"))
      (is (equal (tenant-id tenant)
                 "52dadc45c0c14e519f5026dbe5259fbc"))
      (is (equal (tenant-enabled tenant)
                 t))
      (is (equal (tenant-description tenant)
                 "test description")))))


(test get-tenant
  "Test the getting a tenant."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"tenant\": {\"description\": \"test description\", \"enabled\": true, \"id\": \"52dadc45c0c14e519f5026dbe5259fbc\", \"name\": \"test123\"}}}")
    (let ((tenant (get-tenant (connection-fixture) "52dadc45c0c14e519f5026dbe5259fbc")))
      (is-valid-request mock-stream :get "/v2.0//tenants/52dadc45c0c14e519f5026dbe5259fbc"
                        :host "192.168.1.9:35357")
      (is (equal (tenant-name tenant)
                 "test123"))
      (is (equal (tenant-id tenant)
                 "52dadc45c0c14e519f5026dbe5259fbc"))
      (is (equal (tenant-enabled tenant)
                 t))
      (is (equal (tenant-description tenant)
                 "test description")))))


(test delete-tenant
  "Test the deleting a tenant."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream 204)
    (let ((tenant (delete-tenant (connection-fixture) "52dadc45c0c14e519f5026dbe5259fbc")))
      (is-valid-request mock-stream :delete "/v2.0//tenants/52dadc45c0c14e519f5026dbe5259fbc"
                        :host "192.168.1.9:35357")
      (is (null tenant)))))


(test list-tenants
  "Test the parsing of a tenants list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"tenants_links\": [], \"tenants\": [{\"description\": null, \"enabled\": true, \"id\": \"010c021c\", \"name\": \"service\"}, {\"description\": null, \"enabled\": true, \"id\": \"39dd2c\", \"name\": \"invisible_to_admin\"}, {\"description\": null, \"enabled\": true, \"id\": \"45ca25c\", \"name\": \"admin\"}, {\"description\": \"test description\", \"enabled\": true, \"id\": \"5dbb9f7\", \"name\": \"alt_demo\"}, {\"description\": null, \"enabled\": false, \"id\": \"968075c\", \"name\": \"demo\"}]}")
    (let ((tenants (list-tenants (connection-fixture))))
      (is-valid-request mock-stream :get "/v2.0//tenants")
      (is (equal (mapcar #'tenant-name tenants)
                 '("service" "invisible_to_admin" "admin"
                   "alt_demo" "demo")))
      (is (equal (mapcar #'tenant-id tenants)
                 '("010c021c" "39dd2c" "45ca25c"
                   "5dbb9f7" "968075c")))
      (is (equal (mapcar #'tenant-enabled tenants)
                 '(t t t t nil)))
      (is (equal (mapcar #'tenant-description tenants)
                 '(nil nil nil "test description" nil))))))

;;
;; Users
;;

(test add-user
  "Test the adding a user."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"user\": {\"name\": \"test\", \"enabled\": true, \"email\": \"test@example.com\", \"id\": \"xxxxxxx\"}}")
    (let ((user (add-user (connection-fixture)
                             :name "test" :email "test@example.com"
                             :password "secret" :enabled t)))
      (is-valid-request mock-stream :post "/v2.0//users"
                        :content
                        "{\"user\":{\"name\":\"test\",\"email\":\"test@example.com\",\"enabled\":true,\"password\":\"secret\"}}"
                        :host "192.168.1.9:35357")

      (is (equal (user-name user)
                 "test"))
      (is (equal (user-id user)
                 "xxxxxxx"))
      (is (equal (user-enabled user)
                 t))
      (is (equal (user-email user)
                 "test@example.com")))))

(test get-user
  "Test the parsing of a get user response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"user\": {\"username\": \"nova\", \"name\": \"nova\", \"id\": \"47ce9d08d7e24ba89307fe280fa66235\", \"enabled\": true, \"email\": null, \"tenantId\": \"d0335d0bad854ebfb97264647523a654\"}}")
    (let ((user (get-user (connection-fixture) "47ce9d08d7e24ba89307fe280fa66235")))
      (is-valid-request mock-stream :get "/v2.0//users/47ce9d08d7e24ba89307fe280fa66235"
                        :host "192.168.1.9:35357")
      (is (equal (user-name user) "nova"))
      (is (equal (user-id user) "47ce9d08d7e24ba89307fe280fa66235"))
      (is (equal (user-enabled user) t))
      (is (equal (user-email user) nil)))))


(test delete-user
  "Test the deleting a user."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream 204)
    (let ((user (delete-user (connection-fixture) "47ce9d08d7e24ba89307fe280fa66235")))
      (is-valid-request mock-stream :delete "/v2.0//users/47ce9d08d7e24ba89307fe280fa66235"
                        :host "192.168.1.9:35357")
      (is (null user)))))


(test list-users
  "Test the parsing of a user list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"users\": [{\"name\": \"admin\", \"enabled\": true, \"email\": \"admin@example.com\", \"id\": \"6d205b8\"}, {\"name\": \"demo\", \"enabled\": false, \"email\": \"demo@example.com\", \"id\": \"db82b12\"}]}")
    (let ((users (list-users (connection-fixture))))
      (is-valid-request mock-stream :get "/v2.0//users"
                        :host "192.168.1.9:35357")
      (is (equal (mapcar #'user-name users)
                 '("admin" "demo")))
      (is (equal (mapcar #'user-id users)
                 '("6d205b8" "db82b12")))
      (is (equal (mapcar #'user-enabled users)
                 '(t nil)))
      (is (equal (mapcar #'user-email users)
                 '("admin@example.com" "demo@example.com"))))))


;;
;; Roles
;;


(test add-role
  "Test the adding a role."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"role\": {\"name\": \"test\", \"description\": \"test description\", \"id\": \"559b760290e346f6b7fa502328f72c64\"}}")
    (let ((role (add-role (connection-fixture)
                             :name "test" :description "test description")))
      (is-valid-request mock-stream :post "/v2.0//OS-KSADM/roles"
                        :content
                        "{\"role\":{\"name\":\"test\",\"description\":\"test description\"}}"
                        :host "192.168.1.9:35357")

      (is (equal (role-id role)
                 "559b760290e346f6b7fa502328f72c64"))
      (is (equal (role-name role)
                 "test"))
      (is (equal (role-description role)
                 "test description")))))

(test get-role
  "Test the parsing of a get role response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"role\": {\"name\": \"test\", \"description\": \"test description\", \"id\": \"47ce9d08d7e24ba89307fe280fa66235\"}}")
    (let ((role (get-role (connection-fixture) "47ce9d08d7e24ba89307fe280fa66235")))
      (is-valid-request mock-stream :get "/v2.0//OS-KSADM/roles/47ce9d08d7e24ba89307fe280fa66235"
                        :host "192.168.1.9:35357")
      (is (equal (role-name role) "test"))
      (is (equal (role-id role) "47ce9d08d7e24ba89307fe280fa66235"))
      (is (equal (role-description role) "test description")))))

(test delete-role
  "Test the deleting a role."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream 204)
    (let ((role (delete-role (connection-fixture) "47ce9d08d7e24ba89307fe280fa66235")))
      (is-valid-request mock-stream :delete "/v2.0//OS-KSADM/roles/47ce9d08d7e24ba89307fe280fa66235"
                        :host "192.168.1.9:35357")
      (is (null role)))))

(test list-roles
  "Test the parsing of a role list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"roles\": [{\"id\": \"9fe2ff9ee4384b1894a90878d3e92bab\", \"name\": \"_member_\"}, {\"id\": \"fe95d236ee0a4c368b38ff0cd831111c\", \"name\": \"anotherrole\"}]}")
    (let ((roles (list-roles (connection-fixture) t)))
      (is-valid-request mock-stream :get "/v2.0//OS-KSADM/roles/"
                        :host "192.168.1.9:35357")
      (is (equal (mapcar #'role-name roles)
                 '("_member_" "anotherrole")))
      (is (equal (mapcar #'role-id roles)
                 '("9fe2ff9ee4384b1894a90878d3e92bab" "fe95d236ee0a4c368b38ff0cd831111c"))))))

(test list-user-roles
  "Test the parsing of a role list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"roles\": [{\"id\": \"9fe2ff9ee4384b1894a90878d3e92bab\", \"name\": \"_member_\"}, {\"id\": \"fe95d236ee0a4c368b38ff0cd831111c\", \"name\": \"anotherrole\"}]}")
    (let ((roles (list-roles (connection-fixture) (user-fixture))))
      (is-valid-request mock-stream :get "/v2.0//users/2c04749/roles"
                        :host "192.168.1.9:35357")
      (is (equal (mapcar #'role-name roles)
                 '("_member_" "anotherrole")))
      (is (equal (mapcar #'role-id roles)
                 '("9fe2ff9ee4384b1894a90878d3e92bab" "fe95d236ee0a4c368b38ff0cd831111c"))))))

(test list-tenant-user-roles
  "Test the parsing of a role list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"roles\": [{\"id\": \"9fe2ff9ee4384b1894a90878d3e92bab\", \"name\": \"_member_\"}, {\"id\": \"fe95d236ee0a4c368b38ff0cd831111c\", \"name\": \"anotherrole\"}]}")
    (let ((roles (list-roles (tenant-fixture) (user-fixture))))
      (is-valid-request mock-stream :get "/v2.0//tenants/2c04749/users/2c04749/roles"
                        :host "192.168.1.9:35357")
      (is (equal (mapcar #'role-name roles)
                 '("_member_" "anotherrole")))
      (is (equal (mapcar #'role-id roles)
                 '("9fe2ff9ee4384b1894a90878d3e92bab" "fe95d236ee0a4c368b38ff0cd831111c"))))))

(test add-users-tenant-role
  "Test the parsing of a role list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream 200
                   :content "{\"role\": {\"name\": \"anotherrole\", \"id\": \"47ce9d08d7e24ba89307fe280fa66235\"}}")
    (let ((role (add-users-tenant-role (tenant-fixture) (user-fixture) "9fe2ff9ee4384b1894a90878d3e92bab")))
      (is-valid-request mock-stream :put "/v2.0//tenants/2c04749/users/2c04749/roles/OS-KSADM/9fe2ff9ee4384b1894a90878d3e92bab"
                        :host "192.168.1.9:35357")
      (is (equal (role-name role) "anotherrole"))
      (is (equal (role-id role) "47ce9d08d7e24ba89307fe280fa66235")))))


(test delete-users-tenant-role
  "Test the parsing of a role list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream 204)
    (let ((role (delete-users-tenant-role (tenant-fixture) (user-fixture) "9fe2ff9ee4384b1894a90878d3e92bab")))
      (is-valid-request mock-stream :delete "/v2.0//tenants/2c04749/users/2c04749/roles/OS-KSADM/9fe2ff9ee4384b1894a90878d3e92bab"
                        :host "192.168.1.9:35357")
      (is (null role)))))
