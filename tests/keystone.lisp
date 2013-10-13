(defpackage cl-keystone-client-test
  (:use fiveam
        cl
        cl-openstack-client-test
        cl-keystone-client)
  (:import-from :local-time
                :encode-timestamp
                :timestamp-to-unix
                :timestamp=
                :timestamp+
                :format-timestring
                :now
                :+utc-zone+))

(in-package :cl-keystone-client-test)

(def-suite keystone :description "My Example Suite")

(in-suite keystone)

(defparameter +keystone-format+
  ;; same as +ISO-8601-FORMAT+ except with non nano seconds.
  '((:year 4) #\- (:month 2) #\- (:day 2) #\T
    (:hour 2) #\: (:min 2) #\: (:sec 2)
    :gmt-offset-or-z))

(defun connection-fixture (&key
                             (url "http://localhost:5000")
                             (username "demo")
                             (password "demo"))
  (make-instance 'connection-v2 :url url
                                :password password
                                :username username))

(test make-connection
      "Make a connection object"
      (is-true (make-instance 'connection-v2)))


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
