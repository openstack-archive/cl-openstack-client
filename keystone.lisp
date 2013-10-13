(defpackage cl-keystone-client
  (:use cl cl-json drakma)
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
           connection-token-expires))

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
                        :stream *cached-stream*
                        :content-type "application/json"
                        :content
                        (with-explicit-encoder
                          (encode-json-to-string
                           `(:object "auth" (:object "passwordCredentials"
                                                     (:object "username" ,username
                                                              "password" ,password)
                                                     ,@tenant-prop)))))
        (declare (ignore must-close reason-phrase body))
        (cond
          ((and (eql status-code 200)
                (json-response-p headers))
           (setf token
                 (cdr (assoc :token (cdr (assoc :access (decode-json stream)))))))
          ((json-response-p headers)
           (json-error (decode-json stream)))
          (t
           (unknown-error uri status-code)))))))

(defgeneric connection-token-id (connection)
  (:documentation "Retrieve token id for CONNECTION."))

(defmethod connection-token-id ((connection connection-v2))
  (cdr (assoc :id (slot-value connection 'token))))


(defgeneric connection-token-expires (connection)
  (:documentation "Retrieve token expiration for CONNECTION."))

(defmethod connection-token-expires ((connection connection-v2))
  (cdr (assoc :expires (slot-value connection 'token))))
