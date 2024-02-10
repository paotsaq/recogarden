(import test)
(import (chicken io))
(import simple-exceptions)
(include "api-consts.scm")
(import (only http-client
			  with-input-from-request
			  call-with-input-request)
		(only intarweb
			  request?
			  response?
			  headers
			  make-request)
		(only medea
			  read-json)
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

; creates the HTTP request object to an API endpoint
; String intarweb#uri -> intarweb#request
(define (make-request-to-endpoint api-key uri-obj)
    (make-request
      #:method 'GET
      #:uri uri-obj
	  #:headers (headers `((Authorization, api-key)))))

; sends the request and gets the JSON response
; URIString -> intarweb#response
(define (get-response-from-endpoint endpoint)
  (print "DOING A REQUEST FOR " endpoint)
  (with-input-from-request (make-request-to-endpoint API-KEY
													 (create-api-uri-object endpoint)) #f read-json))
