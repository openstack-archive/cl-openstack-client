(defpackage cl-keystone-client
  (:use cl)
  (:import-from #:cl-openstack-client
                #:*http-stream*
                #:assoc*
                #:decode-resource
                #:decode-resource-list
                #:def-rest-method
                #:error-code
                #:error-message
                #:handle-http-error
                #:id
                #:openstack-error
                #:request-resource
                #:resource
                #:resource-authentication-headers
                #:resource-connection
                #:resource-error-class
                #:resource-id
                #:service-url)
  (:import-from #:drakma
                #:http-request)
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
                #:alist-plist)
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
           connection-tenant
           connection-service-catalog

           ;; Resource Methods
           resource-id
           resource-name
           resource-connection
           resource-description

           ;; Resource Slots
           id
           name
           enabled
           description

           ;; Tenant Methods
           tenant
           tenant-id
           tenant-name
           tenant-enabled
           tenant-description
           list-tenants

           ;; User Methods
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

           ;; Role Methods
           role-id
           role-name
           role-enabled
           list-roles))

(in-package :cl-keystone-client)

(define-condition keystone-error (openstack-error) ())

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
   (token)
   (user)
   (metadata)
   (service-catalog :reader connection-service-catalog)
   (url :initarg :url
        :reader connection-url
        :initform (error ":URL is required when creating a connection."))))

;; Add API compatability with the resource object
(defmethod resource-connection ((connection connection))
  connection)

(defmethod resource-error-class ((resource connection))
  'keystone-error)

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

(defgeneric authenticate (connection)
  (:documentation "Authenticate and retrieve a token."))

(defmethod authenticate ((connection connection-v2))
  (with-slots (url token user service-catalog metadata) connection
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request (format nil "~a/v2.0/tokens" url)
                      :method :POST
                      :want-stream t
                      :stream *http-stream*
                      :content-type "application/json"
                      :content
                      (encode-json-to-string connection))
      (declare (ignore must-close reason-phrase body))
      (handle-http-error connection uri status-code headers stream)
      (let ((access (assoc* :access (decode-json stream))))
        (setf user (assoc* :user access))
        (setf service-catalog (assoc* :service-catalog access))
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
  (parse-timestring (assoc* :issued-at (slot-value connection 'token))))

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

(defmethod connection-tenant ((connection connection-v2))
  "Return the current connections TENANT."
  (apply #'make-instance
         'tenant-v2
         :connection connection
         (alist-plist (assoc* :tenant (slot-value connection 'token)))))

(defmethod resource-authentication-headers ((resource connection-v2))
  `(("x-auth-token" . ,(connection-token-id resource))))

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


(defclass resource-v2 (resource)
  ())

(defmethod resource-error-class ((resource resource-v2))
  'keystone-error)

(defmethod resource-authentication-headers ((resource resource-v2))
  (resource-authentication-headers (resource-connection resource)))

(defmethod service-url ((resource resource-v2) &optional (service "identity"))
  (service-url (resource-connection resource) service))


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
  ((name :initarg :name :reader user-name)
   (tenant-id :initarg :tenant-id :reader user-tenant)
   (enabled :initarg :enabled :reader user-enabled)
   (email :initarg :email :reader user-email)))

(defmethod user-id ((user user))
  (resource-id user))

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
