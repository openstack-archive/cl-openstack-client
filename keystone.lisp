(defpackage cl-keystone-client
  (:use cl)
  (:import-from #:cl-openstack-client
                #:*http-stream*
                #:assoc*
                #:decode-resource
                #:decode-resource-list
                #:decode-json
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
                #:alist-plist
                #:if-let)
  (:export keystone-error
           error-code
           error-message

           ;; Connection methods
           connection
           connection-v2
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
           connection-user
           connection-service-catalog
           connection-api-details
           list-versions
           authenticate

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
           tenant-v2
           tenant-id
           tenant-name
           tenant-enabled
           tenant-description
           add-tenant
           get-tenant
           delete-tenant
           list-tenants

           ;; User Methods
           user
           user-v2
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
           role
           role-v2
           role-id
           role-name
           role-description
           role-enabled
           add-role
           get-role
           delete-role
           list-roles
           add-users-tenant-role
           delete-users-tenant-role

           ;; Service Methods
           service-v2
           service-type
           service-enabled
           service-description
           add-service
           get-service
           delete-service
           list-services))

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

;; Add API compatibility with the resource object
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

(defgeneric authenticate (connection)
  (:documentation "Authenticate and retrieve a token."))

(defmethod authenticate ((connection connection-v2))
  (with-slots (url token user service-catalog metadata) connection
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request (format nil "~a/v2.0/tokens" url)
                      :method :post
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
  (unless (slot-boundp connection 'token)
    (error "Connection is not authenticated."))
  (apply #'make-instance
         'tenant-v2
         :connection connection
         (alist-plist (assoc* :tenant (slot-value connection 'token)))))

(defmethod connection-user ((connection connection-v2))
  "Return the current connections TENANT."
  (unless (slot-boundp connection 'token)
    (error "Connection is not authenticated."))
  (get-user connection
            (assoc* :id (slot-value connection 'user))))

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

(defmethod service-url ((connection connection-v2) &key
                                                     (service-name "identity")
                                                     endpoint-type)
  (car (service-catalog-query
        connection
        service-name
        :type (or endpoint-type (connection-endpoint connection)))))


(defclass resource-v2 (resource)
  ())

(defmethod resource-error-class ((resource resource-v2))
  'keystone-error)

(defmethod resource-authentication-headers ((resource resource-v2))
  (resource-authentication-headers (resource-connection resource)))

(defmethod service-url ((resource resource-v2) &key
                                                 (service-name "identity")
                                                 endpoint-type)
  (car (service-catalog-query
        (resource-connection resource)
        service-name
        :type (or endpoint-type (connection-endpoint (resource-connection resource))))))


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

;; API Versions
(defgeneric list-versions (resource))

(defmethod list-versions ((connection connection-v2))
  "Lists information about all Compute API versions."
  (with-slots (url) connection
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request (format nil "~a" url)
                      :method :GET
                      :want-stream t
                      :accept "application/json"
                      :stream *http-stream*)
      (declare (ignore must-close reason-phrase body))
      (handle-http-error connection uri status-code headers stream :success-codes '(200 300))
      (decode-json stream))))


(def-rest-method connection-api-details ((connection connection))
    ((:documentation "Shows details for the Identity API.")
     (:uri "/"))
  (let ((json (request-resource connection :method :get)))
    json))

;;
;; Tenants
;;

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

(defgeneric add-tenant (connection &key name description enabled))

(def-rest-method add-tenant ((connection connection-v2) &key name description (enabled t))
    ((:documentation "Add a tenant.")
     (:endpoint-type :admin-url)
     (:uri "/tenants"))
  (let ((json (request-resource
               connection
               :method :post
               :content (with-output-to-string (stream)
                          (with-explicit-encoder
                            (encode-json
                             `(:object
                               :tenant
                               (:object
                                :name ,name
                                :description ,description
                                :enabled ,enabled))
                             stream))))))
    (decode-resource (assoc* :tenant json)
                     connection
                     'tenant-v2)))


(defgeneric get-tenant-by-name (resource &key name))
(def-rest-method get-tenant-by-name ((connection connection-v2) &key name)
    ((:documentation "Get a Tenant by name.")
     (:endpoint-type :admin-url)
     (:uri "/tenants")
     (:query (name)))
  (let ((json (request-resource connection :method :get)))
    (decode-resource (assoc* :tenant json)
                     connection 'tenant-v2)))


(defgeneric get-tenant (resource tenant-id-or-name))

(def-rest-method get-tenant (connection tenant-id-or-name)
    ((:documentation "Get tenant information by id or name.")
     (:endpoint-type :admin-url)
     (:uri "/tenants/{tenant-id-or-name}"))

  (handler-case
      (let ((json (request-resource connection :method :get)))
        (decode-resource (assoc* :tenant json)
                         connection
                         'tenant-v2))
    (keystone-error (ks-error)
      (if (eq (error-code ks-error) 404)
          (get-tenant-by-name connection :name tenant-id-or-name)
          (error ks-error)))))


(defgeneric delete-tenant (resource tenant-or-tenant-id))

(def-rest-method delete-tenant ((connection connection-v2) tenant-or-tenant-id)
    ((:documentation "Delete a tenant.")
     (:endpoint-type :admin-url)
     (:uri "/tenants/{tenant-or-tenant-id}"))
  (request-resource connection :method :delete))


(defgeneric list-tenants (resource &key as-admin))

(def-rest-method list-tenants ((connection connection-v2) &key as-admin)
    ((:documentation "List all the tenants.")
     (:endpoint-type (when as-admin :admin-url))
     (:uri "/tenants"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :tenants json)
                          connection 'tenant-v2)))

;;
;; Users
;;

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

(defgeneric add-user (connection &key name email enabled password))

(def-rest-method add-user ((connection connection-v2) &key name email (enabled t) password)
    ((:documentation "Add a user.")
     (:endpoint-type :admin-url)
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

(def-rest-method get-user (connection user)
  ((:documentation "Gets information for a specified user.")
   (:endpoint-type :admin-url)
   (:uri "/users/{user}"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource (assoc* :user json)
                     connection
                     'user-v2)))

(defgeneric delete-user (resource user-or-user-id))

(def-rest-method delete-user ((connection connection-v2) user-or-user-id)
    ((:documentation "Delete a user.")
     (:endpoint-type :admin-url)
     (:uri "/users/{user-or-user-id}"))
  (request-resource connection :method :delete))

(defgeneric list-users (resource))

(def-rest-method list-users ((tenant tenant-v2))
    ((:documentation "List all the users for tenant.")
     (:endpoint-type :admin-url)
     (:uri "/tenants/{tenant}/users"))
  (let ((json (request-resource tenant
                                :method :get)))
    (decode-resource-list (assoc* :users json)
                          tenant
                          'user-v2)))

(def-rest-method list-users ((connection connection-v2))
    ((:documentation "List all users in keystone.")
     (:endpoint-type :admin-url)
     (:uri "/users"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :users json)
                          connection
                          'user-v2)))

;;
;; Roles
;;

(defclass role (named-resource-v2)
  ((id :initarg :id :reader role-id)
   (description :initarg :description :reader role-description)
   (name :initarg :name :reader role-name)))

(defclass role-v2 (role)
  ())

(defgeneric add-role (context &key name description))

(def-rest-method add-role ((connection connection-v2) &key name (description ""))
    ((:documentation "Add a role.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/roles"))
  (let ((json (request-resource
               connection
               :method :post
               :content (with-output-to-string (stream)
                          (with-explicit-encoder
                            (encode-json
                             `(:object
                               :role
                               (:object
                                :name ,name
                                :description ,description))
                             stream))))))
    (decode-resource (assoc* :role json) connection 'role-v2)))

(defgeneric get-role (context role-id))

(def-rest-method get-role ((connection connection-v2) role-id)
    ((:documentation "Get a role by ID.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/roles/{role-id}"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource (assoc* :role json) connection 'role-v2)))

(defgeneric delete-role (context role-id))

(def-rest-method delete-role ((connection connection-v2) role-or-role-id)
    ((:documentation "Delete a role.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/roles/{role-or-role-id}"))
  (request-resource connection :method :delete))

(defgeneric list-roles (context resource))

(def-rest-method list-roles ((connection connection-v2) resource)
    ((:documentation "List roles.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/roles/"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :roles json) connection 'role-v2)))


(def-rest-method list-roles ((connection connection-v2) (user user-v2))
    ((:documentation "Lists global roles for a specified user. Excludes
tenant roles.")
     (:endpoint-type :admin-url)
     (:uri "/users/{user}/roles"))
  (let ((json (request-resource user :method :get)))
    (decode-resource-list (assoc* :roles json) user 'role-v2)))


(def-rest-method list-roles ((tenant tenant-v2) (user user-v2))
    ((:documentation "Lists roles a user has in a specific tenant.")
     (:endpoint-type :admin-url)
     (:uri "/tenants/{tenant}/users/{user}/roles"))
  (let ((json (request-resource user :method :get)))
    (decode-resource-list (assoc* :roles json) user 'role-v2)))

(defgeneric add-users-tenant-role (tenant user role))

(def-rest-method add-users-tenant-role (tenant user role)
    ((:documentation "Adds a specified role to a user for a tenant.")
     (:endpoint-type :admin-url)
     (:uri "/tenants/{tenant}/users/{user}/roles/OS-KSADM/{role}"))
  (let ((json (request-resource tenant :method :put)))
    (decode-resource (assoc* :role json) (resource-connection tenant) 'role-v2)))

(defgeneric delete-users-tenant-role (tenant user role))

(def-rest-method delete-users-tenant-role (tenant user role)
    ((:documentation "Deletes a specified role from a user on a tenant.")
     (:endpoint-type :admin-url)
     (:uri "/tenants/{tenant}/users/{user}/roles/OS-KSADM/{role}"))
  (request-resource tenant :method :delete))



;;
;; Services
;;

(defclass service-v2 (named-resource-v2)
  ((type :initarg :type :reader service-type)
   (enabled :initarg :enabled :reader service-enabled)
   (description :initarg :description :reader service-description)))

(defgeneric add-service (context &key name type description))

(def-rest-method add-service ((connection connection-v2) &key name type (description ""))
    ((:documentation "Add a service.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/services"))
  (let ((json (request-resource
               connection
               :method :post
               :content (with-output-to-string (stream)
                          (with-explicit-encoder
                            (encode-json
                             `(:object
                               "OS-KSADM:service"
                               (:object
                                :name ,name
                                :type ,type
                                :description ,description))
                             stream))))))
    (decode-resource (assoc* :OS-KSADM--service json) connection 'service-v2)))


(defgeneric get-service (resource service-id))

(def-rest-method get-service ((connection connection-v2) service-id)
    ((:documentation "Get a service.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/services/{service-id}"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource (assoc* :OS-KSADM--service json) connection 'service-v2)))


(defgeneric delete-service (context service-or-service-id))

(def-rest-method delete-service ((connection connection-v2) service-or-service-id)
    ((:documentation "Delete a service.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/services/{service-or-service-id}"))
  (request-resource connection :method :delete))


(defgeneric list-services (resource))

(def-rest-method list-services ((connection connection-v2))
    ((:documentation "List services.")
     (:endpoint-type :admin-url)
     (:uri "/OS-KSADM/services/"))
  (let ((json (request-resource connection :method :get)))
    (decode-resource-list (assoc* :OS-KSADM--services json) connection 'service-v2)))
