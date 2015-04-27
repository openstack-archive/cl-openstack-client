(defpackage cl-openstack-client
  (:use cl)
  (:export #:*resource-uri*
           #:*http-stream*
           #:*debug-stream*

           ;; REST resource definitions
           #:def-rest-method
           #:def-rest-generic

           ;; Error handling
           #:openstack-error
           #:handle-http-error
           #:error-code
           #:error-message

           ;; Resources
           #:resource
           #:resource-connection
           #:resource-attribute
           #:resource-attribute-list
           #:resource-authentication-headers
           #:resource-error-class
           #:decode-resource-list
           #:request-resource
           #:decode-resource
           #:service-url
           #:resource-id

           ;; Resource Slots
           #:id

           ;; Generic Utilities
           #:decode-json
           #:assoc*)
  (:import-from #:drakma
                #:http-request)
  (:import-from #:cl-json
                #:*json-input*
                #:*json-identifier-name-to-lisp*
                #:encode-json
                #:encode-json-to-string)
  (:import-from #:alexandria
                #:alist-hash-table
                #:hash-table-keys
                #:alist-plist)
  (:import-from #:uri-template
                #:uri-template
                #:read-uri-template)
  (:import-from #:flexi-streams
                #:make-flexi-stream
                #:octets-to-string
                #:make-in-memory-input-stream))


(in-package :cl-openstack-client)

(defun assoc* (item alist &rest rest &key key test test-not)
  "Return the CDR of the ASSOC result."
  (declare (ignore key test test-not))
  (cdr (apply #'assoc item alist rest)))

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
            :do (cond
                  ((member c (list #\_))
                   (princ "-" result))
                  ((member c (list #\:))
                   (princ "--" result))
                  (t (princ (char-upcase c) result)))))))

(defun decode-json (&optional (stream *json-input*))
  (let ((*json-identifier-name-to-lisp* #'openstack-camel-case-to-lisp))
    (cl-json:decode-json stream)))

;;; REST method helpers

(defvar *http-stream* nil
  "This stream is primarily used for dependency injection in
  testcases.")

(defvar *debug-stream* nil
  "This stream is used for debugging.  Should be used in combination
with the drakma *HEADERS-STREAM*.")

(define-condition openstack-error (error)
  ((message
    :initarg :message
    :accessor error-message
    :initform nil
    :documentation "The error message returned by Openstack.")
   (code
    :initarg :code
    :accessor error-code
    :initform nil
    :documentation "The error code returned by Openstack."))
  (:report (lambda (condition stream)
             (format stream "Openstack ERROR: ~A, ~A"
                     (error-code condition)
                     (error-message condition)))))

(defun json-error (resource json)
  "Raise an error using the contents of a JSON error plist."
  (let ((error-message (assoc* :error json)))
    (error (resource-error-class resource)
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

(defun handle-http-error (resource uri status-code headers stream
                          &key (success-codes '(200 201)))
  (block nil
    (cond
      ((and (member status-code success-codes)
            (json-response-p headers))
       (return))
      ;; No body
      ((eq status-code 204)
       (return))
      ((json-response-p headers)
       (json-error resource (decode-json stream)))
      (t
       (unknown-error uri status-code)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun resource-id-or-value (item)
    "If the ITEM is a resource then resolve it to a ID, otherwise return
the item."
    (if (subtypep (class-of item) (find-class 'resource))
        (resource-id item)
        item)))

(defun convert-header-resources (headers)
  "Take a list of headers and resolve any RESOURCE types to their
RESOURCE-ID's"
  (loop :for (header . value) :in headers
        :collect (cons header (resource-id-or-value value))))

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
            :collect `(,element (resource-id-or-value ,element)))))

;; Resources act as a base class for all types.

(defclass resource ()
  ((id :initarg :id
       :reader resource-id)
   (connection :initarg :connection
               :reader resource-connection)
   (attributes :initarg :attributes
               :initform (make-hash-table))))

(defmethod resource-error-class ((resource resource))
  'openstack-error)

(defmethod print-object ((resource resource) stream)
  (if (slot-boundp resource 'id)
      (print-unreadable-object (resource stream :type t :identity t)
        (format stream "~A" (resource-id resource)))
      (print-unreadable-object (resource stream :type t :identity t))))

(defmethod decode-resource (resource parent type)
  ;; TODO (RS) currently extra keys are just ignored in all resources,
  ;; it would be best if they were saved somewhere.  This will need to
  ;; be added to support extensions that tack extra data on.
  (apply #'make-instance
         type
         :connection (resource-connection parent)
         :parent parent
         :attributes (alist-hash-table resource)
         (concatenate 'list
                      (alist-plist resource)
                      '(:allow-other-keys t))))

(defmethod decode-resource-list (resources parent type)
  (loop :for resource :in resources
        :collect (decode-resource resource parent type)))

(defgeneric resource-authentication-headers (resource)
  (:documentation "Return a list of the authentication headers that
  should be added to the request."))


(defgeneric resource-attribute (resource attribute))

(defmethod resource-attribute ((resource resource) attribute)
  (gethash attribute (slot-value resource 'attributes)))

(defgeneric resource-attribute-list (resource))

(defmethod resource-attribute-list ((resource resource))
  (hash-table-keys (slot-value resource 'attributes)))

(defvar *resource-uri* nil)

(defun request-resource (resource &key method additional-headers content
                                    (uri *resource-uri*)
                                    (content-type "application/json"))
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (http-request uri
                    :method method
                    :content-type "application/json"
                    :stream *http-stream*
                    :additional-headers
                    (concatenate 'list
                                 (resource-authentication-headers resource)
                                 (convert-header-resources additional-headers))
                    :content (cond
                               ((null content)
                                nil)
                               ((stringp content)
                                content)
                               (t
                                (encode-json-to-string content))))
    (declare (ignore stream must-close reason-phrase))
    (let ((stream (make-flexi-stream (make-in-memory-input-stream body) :external-format :utf-8)))
      (handle-http-error resource uri status-code headers stream))
    (cond
      ((eq status-code 204)
       nil)
      ((equal content-type "application/json")
       (when *debug-stream*
         (write-sequence (octets-to-string body :external-format :utf-8) *debug-stream*)
         (terpri *debug-stream*)
         (terpri *debug-stream*))
       (let ((stream (make-flexi-stream (make-in-memory-input-stream body) :external-format :utf-8)))
         (decode-json stream)))
      (t body))))

(defgeneric service-url (resource &key service-name endpoint-type))

(defun url-join (&rest parts)
  (format nil "~{~A~^/~}" parts))

(defun build-query-string (url &rest query-string)
  (if query-string
      (format nil "~a?~{~(~a~)=~(~a~)~^&~}" url query-string)
      url))

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
is supported from the RFC.  The resulting URI will be bound to the
*RESOURCE-URI* variable for use within other helper functions.

DOCUMENTATION a documentation string that will be assigned to the
method.

BODY is a for the method body.
"
  (let ((uri (or (cadr (assoc :uri options))
                 (error ":URI is required.")))
        (service-type (when (assoc :service-type options)
                        (list :service-name (cadr (assoc :service-type options)))))
        (endpoint-type (when (assoc :endpoint-type options)
                         (list :endpoint-type (cadr (assoc :endpoint-type options)))))
        (documentation (cdr (assoc :documentation options)))
        (query (loop
                 :for item :in (cadr (assoc :query options))
                 :for (q v) = (cond
                                ((listp item) item)
                                (t (list (symbol-name item)
                                         item)))
                 :collect (list 'list q v))))
    `(defmethod ,name ,lambda-list
       ,@documentation
       (let ((*resource-uri*
               (apply
                #'build-query-string
                (url-join
                 (apply #'service-url
                        ,(car (apply #'lambda-list-variables lambda-list))
                        (list ,@service-type ,@endpoint-type))
                 (let ,(apply #'convert-lambda-list-resources lambda-list)
                   (declare (ignorable ,@(apply #'lambda-list-variables lambda-list)))
                   (uri-template
                    ,@(with-input-from-string (stream uri)
                        (read-uri-template stream t)))))
                (loop
                  :for (q v) :in (list ,@query)
                  :for id = (resource-id-or-value v)
                  :append (list q id)))))
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
