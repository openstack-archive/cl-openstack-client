(defpackage cl-keystone-client.test
  (:use fiveam
        cl
        cl-keystone-client)
  (:import-from #:drakma
                #:header-value)
  (:import-from #:cl-openstack-client.test
                #:connection-fixture
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
                #:+utc-zone+)
  (:import-from :cl-ppcre
                #:regex-replace-all))

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
          '((:issued--at . "2013-10-13T06:01:36.315343")
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


(test list-users
  "Test the parsing of a user list response."
  (with-mock-http-stream (mock-stream)
    (mock-response mock-stream
                   200
                   :content "{\"users\": [{\"name\": \"admin\", \"enabled\": true, \"email\": \"admin@example.com\", \"id\": \"6d205b8\"}, {\"name\": \"demo\", \"enabled\": false, \"email\": \"demo@example.com\", \"id\": \"db82b12\"}]}")
    (let ((users (list-users (connection-fixture))))
      (is-valid-request mock-stream :get "/v2.0//users")
      (is (equal (mapcar #'user-name users)
                 '("admin" "demo")))
      (is (equal (mapcar #'user-id users)
                 '("6d205b8" "db82b12")))
      (is (equal (mapcar #'user-enabled users)
                 '(t nil)))
      (is (equal (mapcar #'user-email users)
                 '("admin@example.com" "demo@example.com"))))))

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
                        "{\"user\":{\"name\":\"test\",\"email\":\"test@example.com\",\"enabled\":true,\"password\":\"secret\"}}")

      (is (equal (user-name user)
                 "test"))
      (is (equal (user-id user)
                 "xxxxxxx"))
      (is (equal (user-enabled user)
                 t))
      (is (equal (user-email user)
                 "test@example.com")))))
