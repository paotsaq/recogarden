(import test)
(import (chicken io))
(import (chicken condition))
(include "api-consts.scm")
(include "mylogger.scm")
(import (only http-client
			  with-input-from-request
			  call-with-input-request*)
		(only (chicken process)
			  process-sleep)
		(only intarweb
			  request?
			  response?
			  headers
			  headers->list
			  response-headers
			  make-request)
		(only medea
			  read-json)
		(only srfi-4
			  read-u8vector)
	    (only uri-common
			  uri?
			  absolute-uri
			  make-uri))


; concatenates all fields into a URI-formatted string
; List[(key . value)] -> String
(define (create-field-string fields)
  (if (null? fields)
	""
	(let* ((key (caar fields))
		   (value (cadar fields))
		   (rest (create-field-string (cdr fields))))
	   (string-append "&" key "=" value rest))))

; manually creates URI string for an API request
; String List[(key . value)] -> URIString
(define (build-uri-with-fields base-uri fields)
  (string-append base-uri
				 (create-field-string fields)))

; creates URI object for API request
; URIString -> intarweb#uri
(define (create-api-uri-object uri-string)
  (absolute-uri uri-string))

; creates the HTTP GET request object to the image endpoint
; intarweb#uri -> intarweb#request
(define (make-request-to-image-uri uri-obj)
    (make-request #:method 'GET #:uri uri-obj))

; creates the HTTP GET request object to an API endpoint
; String intarweb#uri -> intarweb#request
(define (make-request-to-endpoint api-key uri-obj)
    (make-request
      #:method 'GET
      #:uri uri-obj
	  #:headers (headers `((Authorization, api-key)))))

; NOTE could this exception handler be refactored?
; sends the request and saves the image file
; URIString filename -> boolean intarweb#uri intarweb#response
(define (request-and-save-image-file endpoint filename)
 	(log-message INFO (string-append "Sending image GET request to " endpoint))
	 (handle-exceptions exn
				   (begin
					 (let ((exn-message ((condition-property-accessor 'exn 'message) exn)))
						 (log-message ERROR exn-message))
					 #f)
				   (call-with-input-request* 
					(make-request-to-image-uri (absolute-uri endpoint))
					#f
; NOTE `response` is not being used; it's one of the advantages of using 
; call-with-input-request*, and it can later be taken advantage of to check for 
; the status code, response-type, etc.
					(lambda (from resp) (saves-image from filename)))))



; sends the request and gets the JSON response
; URIString -> intarweb#response
(define (get-response-from-endpoint endpoint)
  (log-message DEBUG (string-append "Sleeping before GET request to " endpoint))
  (process-sleep 1)
  (log-message INFO (string-append "Sending GET request to " endpoint))
  (handle-exceptions exn
				   (begin
					 (let ((exn-message ((condition-property-accessor 'exn 'message) exn)))
						 (log-message ERROR exn-message))
					 #f)
  					(with-input-from-request
					  (make-request-to-endpoint API-KEY (create-api-uri-object endpoint)) #f read-json)
				   ))
