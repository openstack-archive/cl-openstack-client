(defpackage cl-keystone-client.test
  (:use fiveam
        cl
        trivial-gray-streams
        cl-openstack-client-test
        cl-keystone-client)
  (:import-from :local-time
                :encode-timestamp
                :timestamp-to-unix
                :timestamp=
                :timestamp+
                :format-timestring
                :now
                :+utc-zone+)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :flexi-streams
                :string-to-octets
                :make-flexi-stream
                :octets-to-string)
  (:import-from :drakma
                :+latin-1+)
  (:import-from :chunga
                :make-chunked-stream))

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

(defun connection-fixture (&key
                             (url "http://localhost:5000")
                             (username "demo")
                             (password "demo"))
  (make-instance 'connection-v2 :url url
                                :password password
                                :username username))

(defclass mock-http-stream (fundamental-binary-input-stream
                            fundamental-binary-output-stream
                            fundamental-character-input-stream
                            fundamental-character-output-stream)
  ((mock-requests :accessor mock-request-stream
                  :initform nil)
   (mock-responses-location :initform 0
                           :accessor mock-response-location)
   (mock-responses :accessor mock-response-stream
                   :initform nil)))

(defmethod stream-read-byte ((stream mock-http-stream))
  (if (<= (length (mock-response-stream stream))
           (mock-response-location stream))
      :eof
      (prog1
          (aref (mock-response-stream stream) (mock-response-location stream))
        (incf (mock-response-location stream)))))

(defmethod stream-write-byte ((stream mock-http-stream) byte)
  (push byte (mock-request-stream stream)))

(defmethod stream-write-char ((stream mock-http-stream) char)
  (push char (mock-request-stream stream)))

(defmethod mock-response ((stream mock-http-stream) response)
  (setf (mock-response-stream stream)
        (string-to-octets
         (regex-replace-all (string #\Newline)
                            response
                            (coerce '(#\Return #\Linefeed) 'string)))))

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
  (let* ((mock-stream (make-instance 'mock-http-stream))
         (cl-keystone-client::*cached-stream*
           (make-flexi-stream (make-chunked-stream mock-stream)
                              :external-format +latin-1+)))
    (mock-response mock-stream
                   "HTTP/1.1 404 Not Found
Vary: X-Auth-Token
Content-Type: application/json
Content-Length: 93
Date: Sat, 12 Oct 2013 23:03:22 GMT
Connection: close

{\"error\": {\"message\": \"The resource could not be found.\", \"code\": 404, \"title\": \"Not Found\"}}
")
    (handler-case
     (authenticate (make-instance 'connection-v2
                                  :tenant-name "test"
                                  :url "http://test"
                                  :username "test"
                                  :password "test"))
      (keystone-error (keystone-error)
        (is (eql (error-code keystone-error)
                 404))))
    ))
