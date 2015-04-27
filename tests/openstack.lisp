(defpackage cl-openstack-client.test
  (:use cl
        trivial-gray-streams
        fiveam)
  (:import-from #:drakma
                #:+latin-1+
                #:header-value
                #:read-http-headers)
  (:import-from #:local-time
                #:encode-timestamp
                #:timestamp+
                #:format-timestring
                #:now)
  (:import-from #:cl-keystone-client
                #:connection-v2
                #:tenant-v2
                #:user-v2)
  (:import-from #:cl-openstack-client
                #:*http-stream*)
  (:import-from #:flexi-streams
                #:string-to-octets
                #:make-flexi-stream
                #:make-in-memory-input-stream
                #:octets-to-string
                #:octet)
  (:import-from #:chunga
                #:make-chunked-stream)
  (:export tests
           connection-fixture
           tenant-fixture
           user-fixture
           with-mock-http-stream
           make-mock-http-stream
           read-mock-request
           mock-http-stream))

(in-package :cl-openstack-client.test)

(def-suite tests
  :description "cl-openstack-client tests")

(defun connection-fixture (&key
                             (url "http://localhost:5000")
                             (username "demo")
                             (password "demo"))

  (let ((connection (make-instance 'connection-v2 :url url
                                                  :password password
                                                  :username username)))
    (setf (slot-value connection 'cl-keystone-client::token)
          `((:issued-at . ,(now))
            (:expires . ,(timestamp+ (now) 24 :hour))
            (:id
             . "MIINUAYJKoZIhvcNAQ==")
            (:tenant
             (:description)
             (:enabled . t)
             (:id . "45ca25c")
             (:name . "admin"))))
    (setf (slot-value connection 'cl-keystone-client::service-catalog)
          '(((:endpoints
              ((:admin-url . "http://192.168.1.9:8774/v2/45ca25c")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:8774/v2/45ca25c")
               (:id . "25210b1")
               (:public-url . "http://192.168.1.9:8774/v2/45ca25c")))
             (:endpoints-links) (:type . "compute") (:name . "nova"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:8776/v2/45ca25c")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:8776/v2/45ca25c")
               (:id . "46d0cc5")
               (:public-url . "http://192.168.1.9:8776/v2/45ca25c")))
             (:endpoints-links) (:type . "volumev2") (:name . "cinder"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:8774/v3")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:8774/v3")
               (:id . "5ed56fb")
               (:public-url . "http://192.168.1.9:8774/v3")))
             (:endpoints-links) (:type . "computev3") (:name . "nova"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:3333")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:3333")
               (:id . "a590747")
               (:public-url . "http://192.168.1.9:3333")))
             (:endpoints-links) (:type . "s3") (:name . "s3"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:9292")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:9292")
               (:id . "010d69f")
               (:public-url . "http://192.168.1.9:9292")))
             (:endpoints-links) (:type . "image") (:name . "glance"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:8776/v1/45ca25c")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:8776/v1/45ca25c")
               (:id . "3698a28")
               (:public-url . "http://192.168.1.9:8776/v1/45ca25c")))
             (:endpoints-links) (:type . "volume") (:name . "cinder"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:8773/services/Admin")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:8773/services/Cloud")
               (:id . "aa700cc")
               (:public-url . "http://192.168.1.9:8773/services/Cloud")))
             (:endpoints-links) (:type . "ec2") (:name . "ec2"))
            ((:endpoints
              ((:admin-url . "http://192.168.1.9:35357/v2.0")
               (:region . "RegionOne")
               (:internal-url . "http://192.168.1.9:5000/v2.0")
               (:id . "2c04749")
               (:public-url . "http://192.168.1.9:5000/v2.0")))
             (:endpoints-links) (:type . "identity") (:name . "keystone"))))
    connection))


(defun tenant-fixture (&key
                         (id "2c04749")
                         (enabled t)
                         (description "test description")
                         (connection (connection-fixture)))
  (make-instance
   'tenant-v2
   :connection connection
   :id id
   :enabled enabled
   :description description))

(defun user-fixture (&key
                       (id "2c04749")
                       (email "test@example.com")
                       (enabled t)
                       (connection (connection-fixture)))
  (make-instance
   'user-v2
   :connection connection
   :id id
   :email email
   :enabled enabled))


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

(defmethod stream-read-char ((stream mock-http-stream))
  (stream-read-byte stream))

(defmethod stream-write-byte ((stream mock-http-stream) byte)
  (push byte (mock-request-stream stream)))

(defmethod stream-write-char ((stream mock-http-stream) char)
  (push char (mock-request-stream stream)))

(defun make-mock-http-stream (&optional (stream (make-instance 'mock-http-stream)))
  (make-flexi-stream (make-chunked-stream stream) :external-format +latin-1+))

(defun mock-response (stream code &key headers (content ""))
  (setf (mock-response-stream stream)
        (string-to-octets
         (with-output-to-string (http-stream)
           (labels ((write-http-line (fmt &rest args)
                      (format http-stream "~?~C~C" fmt args #\Return #\Linefeed))
                    (write-header (name value-fmt &rest value-args)
                      (write-http-line "~A: ~?" name value-fmt value-args)))
             (write-http-line "HTTP/1.1 ~D" code)
             (loop :for (header . value) :in headers
                   :do (write-header header "~A" value))
             (write-header "Content-Type" "~A" "application/json")
             (write-header "Content-Length" "~D" (length content))
             (write-header "Connection" "~A" "close")
             (format http-stream "~C~C" #\Return #\Linefeed)
             (write-string content http-stream))))))

(defun read-status-line (stream)
  (let* ((line (or (chunga:read-line* stream)
                   (error "No status line")))
         (first-space-pos (or (position #\Space line :test #'char=)
                              (error "No space in status line ~S." line)))
         (second-space-pos (position #\Space line
                                     :test #'char=
                                     :start (1+ first-space-pos))))
    (list
     (cond ((string-equal line "POST" :end1 first-space-pos) :post)
           ((string-equal line "GET" :end1 first-space-pos) :get)
           ((string-equal line "DELETE" :end1 first-space-pos) :delete)
           ((string-equal line "PUT" :end1 first-space-pos) :put)
           ((string-equal line "PATCH" :end1 first-space-pos) :patch)
           (t (error "Unknown protocol in ~S." line)))
     (cond ((string-equal line "HTTP/1.0" :start1 (1+ second-space-pos)) :http/1.0)
           ((string-equal line "HTTP/1.1" :start1 (1+ second-space-pos)) :http/1.1)
           (t (error "Unknown protocol in ~S." line)))
     (subseq line (1+ first-space-pos) second-space-pos))))

(defmethod read-mock-request ((stream mock-http-stream))
  "Read a request out of a MOCK-HTTP-STREAM.  The result is a list in
form (parsed-status-line headers contents)"
  (let ((http-stream (make-in-memory-input-stream
                      (reverse
                       (slot-value stream 'mock-requests)))))
    (destructuring-bind (method protocol uri)
        (read-status-line http-stream)
      (let ((headers (read-http-headers http-stream)))
        (list (list :method method :protocol protocol :uri uri)
              headers
              (when (header-value :content-length headers)
                (let ((result (make-array (parse-integer (header-value :content-length headers))
                                          :element-type 'octet)))
                  (read-sequence result http-stream)
                  (octets-to-string result))))))))


(defmacro with-mock-http-stream ((stream) &body body)
  `(let* ((,stream (make-instance 'mock-http-stream))
          (*http-stream* (make-flexi-stream (make-chunked-stream ,stream)
                                            :external-format +latin-1+)))
     ,@body))


(defun is-valid-request (stream method uri &key content (host "192.168.1.9:5000"))
  (destructuring-bind (status headers content1)
      (read-mock-request stream)
    (is (equal content1
               content))
    (when (header-value :content-length headers)
      (is (string-equal "application/json"
                        (header-value :content-type headers))))
    (is (string-equal "MIINUAYJKoZIhvcNAQ=="
                      (header-value :x-auth-token headers)))
    (is (string-equal host
                      (header-value :host headers)))
    (is (eql (getf status :method) method))
    (is (string-equal (getf status :uri) uri))))
