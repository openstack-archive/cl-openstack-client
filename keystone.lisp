(defpackage cl-keystone-client
  (:use cl cl-json drakma)
  (:import-from :local-time
                :parse-timestring
                :timestamp>
                :now)
  (:export connection-v2
           authenticate
           keystone-error
           error-code
           error-message
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
           :password ,password)
          ,@(cond
              ((slot-boundp connection 'tenant-id)
               (list :tenant-id (connection-tenant-id connection)))
              ((slot-boundp connection 'tenant-name)
               (list :tenant-name (connection-tenant-name connection))))))
       stream))))

(defclass connection-v2 (connection)
  ((version :initform 2 :reader connection-version)))

(defvar *cached-stream* nil)

(define-condition keystone-error (error)
  ((message
    :initarg :message
    :accessor error-message
    :initform nil
    :documentation "The error message returned by keystone.")
   (code
    :initarg :code
    :accessor error-code
    :initform nil
    :documentation "The error code returned by keystone."))
  (:report (lambda (condition stream)
             (format stream "Keystone ERROR: ~A, ~A"
                     (error-code condition)
                     (error-message condition)))))

(defun json-error (json)
  "Raise an error using the contents of a JSON error plist."
  (let ((error-message (cdr (assoc :error json))))
    (error 'keystone-error
           :message (cdr (assoc :message error-message))
           :code (cdr (assoc :code error-message)))))

(defun unknown-error (url status-code)
  "Raise an error with the url and status code."
  (error (format nil "ERROR: received response code of ~A when accessing ~A"
                 status-code url)))

(defun json-response-p (headers)
  "Return true if the response content type is json."
  (string-equal (cdr (assoc :content-type headers))
                "application/json"))

(defgeneric authenticate (connection)
  (:documentation "Authenticate and retrieve a token."))

(defmethod authenticate ((connection connection-v2))
  (with-slots (url token) connection
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request (format nil "~a/v2.0/tokens" url)
                      :method :POST
                      :want-stream t
                      :stream *cached-stream*
                      :content-type "application/json"
                      :content
                      (encode-json-to-string connection))
      (declare (ignore must-close reason-phrase body))
      (cond
        ((and (eql status-code 200)
              (json-response-p headers))
         (setf token
               (cdr (assoc :token (cdr (assoc :access (decode-json stream)))))))
        ((json-response-p headers)
         (json-error (decode-json stream)))
        (t
         (unknown-error uri status-code))))))

(defgeneric connection-token-id (connection)
  (:documentation "Retrieve token id for CONNECTION."))

(defmethod connection-token-id ((connection connection-v2))
  (cdr (assoc :id (slot-value connection 'token))))

(defgeneric connection-token-issued-at (connection)
  (:documentation "Return the time the CONNECTION's token was issued
at."))

(defmethod connection-token-issued-at ((connection connection-v2))
  (parse-timestring (cdr (assoc :issued--at (slot-value connection 'token)))))

(defgeneric connection-token-expires (connection)
  (:documentation "Return the time when the CONNECTION's token will
expire."))

(defmethod connection-token-expires ((connection connection-v2))
  (parse-timestring (cdr (assoc :expires (slot-value connection 'token)))))

(defgeneric connection-token-valid-p (connection)
  (:documentation "Return T if the CONNECTION's token is still
valid."))

(defmethod connection-token-valid-p ((connection connection-v2))
  (timestamp>
   (connection-token-expires connection)
   (now)))
