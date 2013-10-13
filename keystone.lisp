(defpackage cl-keystone-client
  (:use cl cl-json drakma)
  (:import-from :local-time
                :parse-timestring
                :timestamp>
                :now)
  (:export connection-v2
           authenticate
           connection-username
           connection-tenant-id
           connection-tenant-name
           connection-password
           connection-url
           connection-token-id
           connection-token-expires
           connection-token-issued-at
           connection-token-valid-p))

(in-package :cl-keystone-client)


(defclass connection ()
  ((username :initarg :username :reader connection-username)
   (tenant-id :initarg :tenant-id :initform nil :reader connection-tenant-id)
   (tenant-name :initarg :tenant-name :initform nil :reader connection-tenant-name)
   (password :initarg :password :reader connection-password)
   (token :initarg :password)
   (url :initarg :url :reader connection-url)))

(defclass connection-v2 (connection)
  ((version :initform 2 :reader connection-version)))


(defgeneric authenticate (connection)
  (:documentation "Authenticate and retrieve a token."))

(defmethod authenticate ((connection connection-v2))
  (with-slots (url token username password tenant-id tenant-name) connection
    (unless (or tenant-id tenant-name)
      (error "No tenant-id nor tenant-name specified, cannot authenticate."))
    (let ((tenant-prop (if tenant-id
                           (list "tenantId" tenant-id)
                         (list "tenantName" tenant-name))))
      (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
          (http-request (format nil "~a/v2.0/tokens" url)
                        :method :POST
                        :want-stream t
                        :content-type "application/json"
                        :content
                        (with-explicit-encoder
                         (encode-json-to-string
                          `(:object "auth" (:object "passwordCredentials"
                                                    (:object "username" ,username
                                                            "password" ,password)
                                                   ,@tenant-prop)))))
        (setf token
              (cdr (assoc :token (cdr (assoc :access (decode-json stream))))))))))


(defgeneric connection-token-id (connection)
  (:documentation "Retrieve token id for CONNECTION."))

(defmethod connection-token-id ((connection connection-v2))
  (cdr (assoc :id (slot-value connection 'token))))


(defgeneric connection-token-expires (connection)
  (:documentation "Retrieve token expiration for CONNECTION."))

(defmethod connection-token-issued-at ((connection connection-v2))
  (parse-timestring (cdr (assoc :issued--at (slot-value connection 'token)))))

(defmethod connection-token-expires ((connection connection-v2))
  (parse-timestring (cdr (assoc :expires (slot-value connection 'token)))))

(defmethod connection-token-valid-p ((connection connection-v2))
  (timestamp>
   (parse-timestring (cdr (assoc :expires (slot-value connection 'token))))
   (now)))
