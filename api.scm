(import test)
(import (chicken io))
(import simple-exceptions)
(include "api-consts.scm")
(import (only http-client
			  with-input-from-request
			  call-with-input-request)
		(only intarweb
			  request?
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
(test "can create EXPECTED-SNATCHED-URI"
  #t (string=? EXPECTED-SNATCHED-URI
			   (build-uri-with-fields SNATCHED-URI
									  (list (list "id" (number->string USER-ID))
											(list "type" "snatched")
											(list "limit" (number->string NBR-OF-SNATCHES))))))
(test "can create EXPECTED-GROUP-URI-WITH-ID-1234"
  #t (string=? EXPECTED-GROUP-URI-WITH-ID-1234
			   (build-uri-with-fields GROUP-URI
									  (list (list "id" (number->string 1234))))))

; creates URI object for API request
; URIString -> intarweb#uri
(define (create-api-uri-object uri-string)
  (absolute-uri uri-string))
(test "creates URI object from string"
	  #t (uri? (create-api-uri-object EXPECTED-SNATCHED-URI)))

; creates the HTTP request object to an API endpoint
; String intarweb#uri -> intarweb#request
(define (make-request-to-endpoint api-key uri-obj)
    (make-request
      #:method 'GET
      #:uri uri-obj
	  #:headers (headers `((Authorization, api-key)))))
(test "can create intarweb#request object"
	  #t (request? (make-request-to-endpoint API-KEY SNATCHED-URI)))

; sends the request and gets the JSON response
; URIString -> intarweb#response
(define (get-response-from-endpoint endpoint)
  (with-input-from-request (make-request-to-endpoint API-KEY
													 (create-api-uri-object endpoint)) #f read-json))
(test "HTTP request to API index is successful"
	  #t (string=? "success" (cdar (get-response-from-endpoint INDEX-URI))))
