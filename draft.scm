(import test)
(import defstruct)
(import (chicken io))
(import simple-exceptions)
(include "api-consts.scm")
(import (only http-client
			  with-input-from-request)
		(only intarweb
			  header-value
			  headers
			  make-request
			  response-code
			  response-headers
			  update-request))

(define NBR-OF-SNATCHES 10)

(defstruct api-object artist album-title year)

; API REQUESTS

; type is one of
; "seeding" / "leeching" / "snatched" / "uploaded"
; userId type limit 
; ommitted offset parameter
(define (build-snatched-url user-id type limit)
  (string-append SNATCHED-URL
				 "&id=" user-id
				 "&type=" type
				 "&limit" limit))


; makes request for latest snatched
(define (make-request-to-snatched )
    (make-request
      #:method 'GET
      #:uri (build-snatched-url USER-ID "snatched" NBR-OF-SNATCHES)
      #:headers (headers `((x-transmission-session-id ,session-id)))))

; TIDDLERS

; makes request for latest snatched
(define (create-tiddler-string-from-api-object api-object)
  '())

(print (make-request-to-snatched))
