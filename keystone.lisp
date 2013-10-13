(defpackage cl-keystone-client
  (:use cl cl-json drakma)
  (:export connection-v2
           authenticate
           connection-username
           connection-tenant-id
           connection-tenant-name
           connection-password
           connection-url
           connection-token-id
           connection-token-expires))

(in-package :cl-keystone-client)


(defclass connection ()
  ((username :initarg :username
             :reader connection-username
             :initform (error ":USERNAME is required when creating a connection."))
   (tenant-id :initarg :tenant-id
              :reader connection-tenant-id)
   (tenant-name :initarg :tenant-name
                :reader connection-tenant-name)
   (password :initarg :password
             :initform (error ":PASSWORD is required when creating a connection.")
             :reader connection-password)
   (token :initarg :password)
   (url :initarg :url
        :reader connection-url
        :initform (error ":URL is required when creating a connection."))))

(defmethod encode-json ((connection connection)
                        &optional (stream json:*json-output*))
  "Write the JSON representation (Object) of the keystone CONNECTION
to STREAM (or to *JSON-OUTPUT*)."
  (with-slots (username password) connection
    (with-explicit-encoder
      (encode-json
       `(:object
         :auth
         (:object
          :password-credentials
          (:object
           :username ,username
           :password ,password))
         ,@(cond
             ((slot-boundp connection 'tenant-id)
              (list :tennant-id (connection-tenant-id connection)))
             ((slot-boundp connection 'tenant-name)
              (list :tennant-name (connection-tenant-name connection)))))
       stream))))

(defclass connection-v2 (connection)
  ((version :initform 2 :reader connection-version)))


(defgeneric authenticate (connection)
  (:documentation "Authenticate and retrieve a token."))

(defmethod authenticate ((connection connection-v2))
  (with-slots (url token) connection
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request (format nil "~a/v2.0/tokens" url)
                      :method :POST
                      :want-stream t
                      :content-type "application/json"
                      :content
                      (encode-json-to-string connection))
      (setf token
            (cdr (assoc :token (cdr (assoc :access (decode-json stream)))))))))


(defgeneric connection-token-id (connection)
  (:documentation "Retrieve token id for CONNECTION."))

(defmethod connection-token-id ((connection connection-v2))
  (cdr (assoc :id (slot-value connection 'token))))


(defgeneric connection-token-expires (connection)
  (:documentation "Retrieve token expiration for CONNECTION."))

(defmethod connection-token-expires ((connection connection-v2))
  (cdr (assoc :expires (slot-value connection 'token))))
