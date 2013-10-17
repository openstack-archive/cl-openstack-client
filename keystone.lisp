(defpackage cl-keystone-client
  (:use cl drakma)
  (:import-from #:cl-openstack-client
                #:assoc*)
  (:import-from #:local-time
                #:parse-timestring
                #:timestamp>
                #:now)
  (:import-from #:cl-json
                #:*json-input*
                #:*json-identifier-name-to-lisp*
                #:with-explicit-encoder
                #:encode-json
                #:encode-json-to-string)
  (:import-from #:alexandria
                #:alist-plist
                #:with-gensyms)
  (:import-from #:uri-template
                #:uri-template
                #:read-uri-template)
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
           connection-token-valid-p
           resource-id
           resource-name
           resource-connection
           tenant-id
           tenant-name
           tenant-enabled
           tenant-description
           list-tenants
           user-id
           user-name
           user-tenant
           user-enabled
           user-email
           user-roles
           add-user
           get-user
           delete-user
           list-users
           role-id
           role-name
           role-enabled
           list-roles))

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
   (endpoint :initarg :endpoint
             :initform :public-url
             :reader connection-endpoint)
   (token :initarg :password)
   (user)
   (tenant)
   (metadata)
   (service-catalog :reader connection-service-catalog)
   (url :initarg :url
        :reader connection-url
        :initform (error ":URL is required when creating a connection."))))

;; Add API compatability with the resource object
(defmethod resource-connection ((connection connection))
  connection)

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

(defmethod headers-for ((connection connection-v2) &optional action)
  (declare (ignore action))
  nil)

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
  (let ((error-message (assoc* :error json)))
    (error 'keystone-error
           :message (assoc* :message error-message)
           :code (assoc* :code error-message))))

(defun unknown-error (url status-code)
  "Raise an error with the url and status code."
  (error (format nil "ERROR: received response code of ~A when accessing ~A"
                 status-code url)))

(defun json-response-p (headers)
  "Return true if the response content type is json."
  (string-equal (assoc* :content-type headers)
                "application/json"))

(defun openstack-camel-case-to-lisp (camel-string)
  "Convert camel case JSON keys to lisp symbol names.  This function
handles keys with names like publicURL better and will convert keys
with underscores to hyphens."
  (declare (string camel-string))
  (let ((*print-pretty* nil))
    (with-output-to-string (result)
      (loop :for c :across camel-string
            :with last-was-lowercase
            :when (and last-was-lowercase
                       (upper-case-p c))
              :do (princ "-" result)
            :if (lower-case-p c)
              :do (setf last-was-lowercase t)
            :else
              :do (setf last-was-lowercase nil)
            :if (member c (list #\_))
              :do (princ "-" result)
            :else
              :do (princ (char-upcase c) result)))))

(defun decode-json (&optional (stream *json-input*))
  (let ((*json-identifier-name-to-lisp* #'openstack-camel-case-to-lisp))
    (cl-json:decode-json stream)))

(defun handle-http-error (uri status-code headers stream)
  (block nil
    (cond
      ((and (member status-code '(200 204))
            (json-response-p headers))
       (return))
      ((json-response-p headers)
       (json-error (decode-json stream)))
      (t
       (unknown-error uri status-code)))))

(defgeneric authenticate (connection)
  (:documentation "Authenticate and retrieve a token."))

(defmethod authenticate ((connection connection-v2))
  (with-slots (url token user service-catalog metadata tenant) connection
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request (format nil "~a/v2.0/tokens" url)
                      :method :POST
                      :want-stream t
                      :stream *cached-stream*
                      :content-type "application/json"
                      :content
                      (encode-json-to-string connection))
      (declare (ignore must-close reason-phrase body))
      (handle-http-error uri status-code headers stream)
      (let ((access (assoc* :access (decode-json stream))))
        (setf user (assoc* :user access))
        (setf service-catalog (assoc* :service-catalog access))
        (setf tenant (assoc* :tenant access))
        (setf metadata (assoc* :metadata access))
        (setf token (assoc* :token access)))))
  connection)

(defgeneric connection-token-id (connection)
  (:documentation "Retrieve token id for CONNECTION."))

(defmethod connection-token-id ((connection connection-v2))
  (assoc* :id (slot-value connection 'token)))

(defgeneric connection-token-issued-at (connection)
  (:documentation "Return the time the CONNECTION's token was issued
at."))

(defmethod connection-token-issued-at ((connection connection-v2))
  (parse-timestring (assoc* :issued--at (slot-value connection 'token))))

(defgeneric connection-token-expires (connection)
  (:documentation "Return the time when the CONNECTION's token will
expire."))

(defmethod connection-token-expires ((connection connection-v2))
  (parse-timestring (assoc* :expires (slot-value connection 'token))))

(defgeneric connection-token-valid-p (connection)
  (:documentation "Return T if the CONNECTION's token is still
valid."))

(defmethod connection-token-valid-p ((connection connection-v2))
  (timestamp>
   (connection-token-expires connection)
   (now)))

;; Service catalog queries

(defun filter-endpoints (endpoints &key (type :public-url) region)
  (loop :for endpoint :in endpoints
        :when (or (not region)
                  (equal (assoc* :region endpoint) region))
          :collect (assoc* type endpoint)))

(defmethod service-catalog-query ((connection connection-v2) service-type &key (type :public-url))
  (loop :for service :in (connection-service-catalog connection)
        :when (equal (assoc* :type service) service-type)
          :append (filter-endpoints (assoc* :endpoints service)
                                    :type type)))

(defmethod service-url ((connection connection-v2) &optional (service "identity"))
  (car (service-catalog-query connection service
                              :type (connection-endpoint connection))))


;;; REST method helpers

(defun convert-header-resources (headers)
  "Take a list of headers and resolve any RESOURCE types to their
RESOURCE-ID's"
  (loop :for (header . value) :in headers
        :when (subtypep (class-of value) (find-class 'resource))
          :collect (cons header (resource-id value))
        :else
          :collect (cons header value)))

(defun return-first-connection (resources)
  (loop :for r :in resources
        :when (or (subtypep (class-of r) (find-class 'resource))
                  (subtypep (class-of r) (find-class 'connection)))
          :return r))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-list-variables (&rest rest)
    (loop :for l :in rest
          :for element = (if (listp l) (car l) l)
          :until (eql (char (symbol-name element) 0) #\&)
          :collect element)))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defun convert-lambda-list-resources (&rest rest)
      (loop :for l :in rest
            :for element = (if (listp l) (car l) l)
            :until (eql (char (symbol-name element) 0) #\&)
            :collect `(,element (if (subtypep (class-of ,element) (find-class 'resource))
                                    (resource-id ,element)
                                    ,element)))))

(defvar *resource-url* nil)

(defmacro def-rest-method (name lambda-list options &body body)
  "A convenience wrapper around request-resource.

NAME is the name of the method. LAMBDA-LIST is a method lambda list,
it's first element will be used to source a connection, so it must be
of the type RESOURCE or CONNECTION.

OPTIONS is in the form of an ALIST and can contain URI or
DOCUMENTATION elements.

URI is the uri to the resource you are looking for it supports
RFC6570‎ tempting and will be evaluated in the context of the method as
if in a PROGN so values from the LAMBDA-LIST will be substituted in
provided the symbol names match.  Any RESOURCE types will have their
RESOURCE-ID methods called before substitution.  Only simple expansion
is supported from the RFC.

DOCUMENTATION a documentation string that will be assigned to the
method.

BODY is a for the method body.
"
  (let ((uri (or (cadr (assoc :uri options))
                 (error ":URI is required.")))
        (documentation (cdr (assoc :documentation options))))
    `(defmethod ,name ,lambda-list
       ,@documentation
       (let ((*resource-url*
               (format nil "~a/~a"
                       (service-url (resource-connection
                                     ,(car (apply #'lambda-list-variables lambda-list))))
                       (let ,(apply #'convert-lambda-list-resources lambda-list)
                         (declare (ignorable ,@(apply #'lambda-list-variables lambda-list)))
                         (uri-template
                          ,@(with-input-from-string (stream uri)
                              (read-uri-template stream t)))))))
         ,@body))))

(defmacro def-rest-generic (name lambda-list &body options)
  "Define a generic with REST methods."
  (let ((documentation (or (cadr (assoc :documentation options)) ""))
        (methods (loop :for body :in options
                       :when (eql (car body) :method)
                         :collect (cdr body))))
    `(progn
       (defgeneric ,name ,lambda-list
         (:documentation ,documentation))
       ,@(loop :for method :in methods
               :collect `(def-rest-method ,name ,@method)))))


;; Resources act as a base class for all types within keystone.

(defclass resource ()
  ((id :initarg :id
       :reader resource-id)
   (connection :initarg :connection
               :reader resource-connection)
   (attributes :initform (make-hash-table))))

(defmethod print-object ((resource resource) stream)
  (if (slot-boundp resource 'id)
      (print-unreadable-object (resource stream :type t :identity t)
        (format stream "~A" (resource-id resource)))
      (print-unreadable-object (resource stream :type t :identity t))))

(defmethod decode-resource (resource parent type)
  (apply #'make-instance
         type
         :connection (resource-connection parent)
         :parent parent
         (concatenate 'list
                      (alist-plist resource)
                      '(:allow-other-keys t))))

(defmethod decode-resource-list (resources parent type)
  (loop :for resource :in resources
        :collect (decode-resource resource parent type)))

(defclass resource-v2 (resource)
  ())

(defmethod service-url ((resource resource-v2) &optional (service "identity"))
  (service-url (resource-connection resource) service))

(defun request-resource (resource &key method additional-headers content
                                    (uri *resource-url*)
                                    (content-type "application/json"))
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (http-request uri
                    :method method
                    :content-type "application/json"
                    :stream *cached-stream*
                    :additional-headers
                    (concatenate 'list
                                 `(("x-auth-token" . ,(connection-token-id
                                                       (resource-connection resource))))
                                 (convert-header-resources additional-headers))
                    :content (cond
                               ((null content)
                                nil)
                               ((stringp content)
                                content)
                               (t
                                (encode-json-to-string content)))
                    :want-stream t)
    (declare (ignore body must-close reason-phrase))
    (handle-http-error uri status-code headers stream)
    (cond
      ((equal content-type "application/json")
       (decode-json stream))
      (t stream))))


(defclass named-resource-v2 (resource-v2)
  ((name :initarg :name :reader resource-name)))

(defmethod print-object ((resource named-resource-v2) stream)
  (if (slot-boundp resource 'name)
      (print-unreadable-object
          (resource stream :type t :identity t)
        (format stream "~a"
                (cond
                  ((and (slot-exists-p resource 'name)
                        (slot-boundp resource 'name))
                   (slot-value resource 'name))
                  ((and (slot-exists-p resource 'id)
                        (slot-boundp resource 'id))
                   (slot-value resource 'id))
                  (t "UNKNOWN"))))
      (print-unreadable-object (resource stream :type t :identity t))))


;; Tenants

(defclass tenant (named-resource-v2)
  ((id :initarg :id :reader tenant-id)
   (name :initarg :name :reader tenant-name)
   (enabled :initarg :enabled :reader tenant-enabled)
   (description :initarg :description :reader tenant-description)))

(defclass tenant-v2 (tenant)
  ())

(defmethod encode-json ((tenant tenant-v2)
                        &optional (stream json:*json-output*))
  "Write the JSON representation (Object) of the keystone CONNECTION
to STREAM (or to *JSON-OUTPUT*)."
  (with-slots (id name enabled description) tenant
    (with-explicit-encoder
      (encode-json
       `(:object
         :tenant
         (:object
          :id ,id
          :name ,name
          :description ,description
          :enabled ,enabled))
       stream))))

(defmethod decode-resource ((type (eql 'tenant-v2)) (parent connection-v2) stream)
  (loop :for tenant :in (assoc* :tenants (decode-json stream))
        :collect (apply #'make-instance
                        type
                        :connection parent
                        (alist-plist tenant))))

(defgeneric list-tenants (resource))

(def-rest-method list-tenants ((connection connection-v2))
    ((:documentation "List all the tenants.")
     (:uri "/tenants"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :tenants json)
                          connection 'tenant-v2)))

;; Users

(defclass user (named-resource-v2)
  ((id :initarg :id :reader user-id)
   (name :initarg :name :reader user-name)
   (tenant-id :initarg :tenant-id :reader user-tenant)
   (enabled :initarg :enabled :reader user-enabled)
   (email :initarg :email :reader user-email)))

(defclass user-v2 (user)
  ())


;;; Make the connection behave like the current user.

(defmethod user-id ((connection connection-v2))
  (assoc* :id (slot-value connection 'user)))

(defmethod user-name ((connection connection-v2))
  (assoc* :name (slot-value connection 'user)))

(defgeneric list-users (resource))

(def-rest-method list-users ((tenant tenant-v2))
  ((:documentation "List all the users for tenant.")
   (:uri "/tenants/{tenant}/users"))
  (let ((json (request-resource tenant
                                :method :get)))
    (decode-resource-list (assoc* :users json)
                          tenant
                          'user-v2)))

(def-rest-method list-users ((connection connection-v2))
    ((:documentation "List all users in keystone.")
     (:uri "/users"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :users json)
                          connection
                          'user-v2)))

(def-rest-method get-user (connection user)
  ((:documentation "Gets information for a specified user.")
   (:uri "/users/{user}"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource (assoc* :user json)
                     connection
                     'user-v2)))

(defgeneric add-user (connection &key name email enabled password))

(def-rest-method add-user ((connection connection-v2) &key name email (enabled t) password)
    ((:documentation "Add a user.")
     (:uri "/users"))
  (let ((json (request-resource
               connection
               :method :post
               :content (with-output-to-string (stream)
                          (with-explicit-encoder
                            (encode-json
                             `(:object
                               :user
                               (:object
                                :name ,name
                                :email ,email
                                :enabled ,enabled
                                :password ,password))
                             stream))))))
    (decode-resource (assoc* :user json)
                     connection
                     'user-v2)))


(defgeneric delete-user (resource user-or-user-id))

(def-rest-method delete-user ((connection connection-v2) user-or-user-id)
  ((:documentation "Delete a user.")
   (:uri "/users/{user-or-user-id}"))
  (request-resource connection :method :delete))

;; Roles

(defclass role (named-resource-v2)
  ((id :initarg :id :reader role-id)
   (name :initarg :name :reader role-name)
   (enabled :initarg :enabled :reader role-enabled)))

(defclass role-v2 (role)
  ())

(defgeneric add-tenant-users-role (tenant user role))

(def-rest-method add-tenant-users-role (tenant user role)
  ((:documentation "Adds a specified role to a user for a tenant.")
   (:uri "/tenants/{tenant}/users/{user}/roles/OS-KSADM/{role}"))
  (request-resource tenant :method :put))

(defgeneric delete-tenants-user-role (tenant user role))

(def-rest-method delete-tenants-user-role (tenant user role)
    ((:documentation "Deletes a specified role from a user on a tenant.")
     (:uri "/tenants/{tenant}/users/{user}/roles/OS-KSADM/{role}"))
  (request-resource tenant :method :delete))

(defgeneric list-roles (resource))

(def-rest-method list-roles ((connection connection-v2))
    ((:documentation "List roles.")
     (:uri "/OS-KSADM/roles/"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :roles json) connection 'role-v2)))


(def-rest-method list-roles ((user user-v2))
  ((:documentation "Lists global roles for a specified user. Excludes
tenant roles.")
   (:uri "/users/{user}/roles"))
  (let ((json (request-resource user :method :get)))
    (decode-resource (assoc* :roles json) user 'role-v2)))
