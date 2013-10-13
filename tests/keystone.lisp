(defpackage cl-keystone-client-test
  (:use fiveam
        cl
        trivial-gray-streams
        cl-openstack-client-test
        cl-keystone-client)
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

(in-package :cl-keystone-client-test)

(def-suite keystone :description "My Example Suite")

(in-suite keystone)

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
      "Make a connection object"
      (is-true (make-instance 'connection-v2)))

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
