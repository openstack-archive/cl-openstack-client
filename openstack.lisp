(defpackage cl-openstack-client
  (:use cl)
  (:export #:*resource-uri*
           #:*http-stream*

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
           #:assoc*)
  (:import-from #:drakma
                #:http-request)
  (:import-from #:cl-json
                #:encode-json
                #:decode-json
                #:encode-json-to-string)
  (:import-from #:alexandria
                #:alist-plist)
  (:import-from #:uri-template
                #:uri-template
                #:read-uri-template))


(in-package :cl-openstack-client)

(defun assoc* (item alist &rest rest &key key test test-not)
  "Return the CDR of the ASSOC result."
  (declare (ignore key test test-not))
  (cdr (apply #'assoc item alist rest)))


;;; REST method helpers

(defvar *http-stream* nil
  "This stream is primarily used for dependency injection in
  testcases.")

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

(defun handle-http-error (resource uri status-code headers stream)
  (block nil
    (cond
      ((and (member status-code '(200 204))
            (json-response-p headers))
       (return))
      ((json-response-p headers)
       (json-error resource (decode-json stream)))
      (t
       (unknown-error uri status-code)))))

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

;; Resources act as a base class for all types.

(defclass resource ()
  ((id :initarg :id
       :reader resource-id)
   (connection :initarg :connection
               :reader resource-connection)
   (attributes :initform (make-hash-table))))

(defmethod resource-error-class ((resource resource))
  'openstack-error)

(defmethod print-object ((resource resource) stream)
  (if (slot-boundp resource 'id)
      (print-unreadable-object (resource stream :type t :identity t)
        (format stream "~A" (resource-id resource)))
      (print-unreadable-object (resource stream :type t :identity t))))

(defmethod decode-resource (resource parent type)
  ;; TODO (RS) currently extra keys are just ignored in all resources,
  ;; it would be best if they were saved somewhere.
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

(defgeneric resource-authentication-headers (resource)
  (:documentation "Return a list of the authentication headers that
  should be added to the request."))

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
                                (encode-json-to-string content)))
                    :want-stream t)
    (declare (ignore body must-close reason-phrase))
    (handle-http-error resource uri status-code headers stream)
    (cond
      ((equal content-type "application/json")
       (decode-json stream))
      (t stream))))

(defgeneric service-url (resource &optional service-name))

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
        (documentation (cdr (assoc :documentation options))))
    `(defmethod ,name ,lambda-list
       ,@documentation
       (let ((*resource-uri*
               (format nil "~a/~a"
                       (service-url ,(car (apply #'lambda-list-variables lambda-list)))
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
