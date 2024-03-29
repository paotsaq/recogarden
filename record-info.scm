(include "api.scm")
(include "utils.scm")
(import (only (chicken file)
			  file-exists?)
		(only medea
			  read-json
			  write-json)
		;(only srfi-67
			  ;list-compare)
		(only srfi-1
			  every
			  reduce))

(define TEST-SNATCHED-FILE "test-snatched-response.json")
(define TEST-GROUPRECORD1234-FILE "test-grouprecord1234-response.json")
(define TEST-GROUPRECORD1926431-FILE "test-grouprecord1926431-response.json")

(define SNATCHED-URI-WITH-FIELDS 
	(build-uri-with-fields SNATCHED-URI
						   (list (list "id" (number->string USER-ID))
								 (list "type" "snatched")
								 (list "limit" (number->string NBR-OF-SNATCHES)))))
(define GROUP-URI-WITH-ID-1234
    (build-uri-with-fields GROUP-URI
						   (list (list "id" (number->string 1234)))))
(define GROUP-URI-WITH-ID-1926431
    (build-uri-with-fields GROUP-URI
						   (list (list "id" (number->string 1926431)))))

; if test-file doesn't exist, loads the json response from a route into a variable
; intarweb#uri -> intarweb#response
(define (load-or-create-test-file test-file api-route) 
  (if (not (file-exists? test-file))
	 (begin
	   (print "test file " test-file " created!")
	   (let ((api-response (get-response-from-endpoint api-route)))
		     (save-obj-to-file api-response test-file)
			 api-response))
	 (begin
	   (print "test file " test-file " loaded!")
	   (with-input-from-file test-file read-json))))

(define snatched-response (load-or-create-test-file TEST-SNATCHED-FILE SNATCHED-URI-WITH-FIELDS))
(define grouprecord-1234-response (load-or-create-test-file TEST-GROUPRECORD1234-FILE GROUP-URI-WITH-ID-1234))
(define grouprecord-1926431-response (load-or-create-test-file TEST-GROUPRECORD1926431-FILE GROUP-URI-WITH-ID-1926431))


; retrieves the artists from a groupid-route-response
; groupid-route-response -> ArtistsVector
(define (retrieve-artists-from-group-response response)
	(cdr (assoc 'artists (cdr (assoc 'musicInfo (cdr (cadadr response)))))))

; creates a string from the artist vector
; ArtistsVector -> String
(define (assemble-artist-string vec)
  (reduce (lambda (a b) (string-append a ", " b))
		  ""
		  (map (lambda (pair) (cdr (assoc 'name pair)))
			   (vector->list vec))))

; retrieves a list of information in the record from the intarweb#response
; groupid-route-response -> record-info
(define (make-record-info-obj-from-groupid-response json-response)
	(make-record-info 
		title: (cdr (assoc 'name (cdr (cadadr json-response))))
		year: (cdr (assoc 'year (cdr (cadadr json-response))))
		artist: (assemble-artist-string (retrieve-artists-from-group-response json-response))
		groupid: (cdr (assoc 'id (cdr (cadadr json-response))))
		image-uri: (cdr (assoc 'wikiImage (cdr (cadadr json-response))))))

; retrieves a vector of records from the intarweb#response
; snatched-route-response -> RecordsVector
(define (get-records json-response)
	(cdadar (cdr json-response)))

; retrieves a list of groupId from each record in RecordsVector
; RecordsVector -> List[groupId]
(define (get-groupids-from-records rec-vec)
  (map (lambda (rec) (cdr (assoc 'groupId rec)))
	   (vector->list rec-vec)))
;; NOTE testing with srfi-67 `list-compare` wasn't successful
;; there's a problem with the function, to maybe tackle later.
;(test "can obtain groupIds from records vector"
	;#t
	;(eq? 0
		 ;(list-compare (get-groupids-from-records (get-records snatched-response))
					   ;(list 372720 36086 337601 881619 2187250 53404 28235 120741 19522 1981603))))

; for each groupId, produce a request to the groupId route
; List[groupId] -> List[intarweb#uri]
(define (get-uris-for-groupids-request groupids)
  (map (lambda (id) (build-uri-with-fields GROUP-URI (list (list "id" (number->string id)))))
	   groupids))


; for each groupId, produce a request to the groupId route
; List[intarweb#uri] -> List[intarweb#response]
(define (get-response-from-group-endpoint groupid-uris)
  (map (lambda (uri) (get-response-from-endpoint uri))
	   groupid-uris))


;; creates a list of record-info objects from a list of groupId responses
;; List[intarweb#response] -> List[record-info]
(define (produces-record-info-list-from-vector-of-records groupid-responses)
  (map (lambda (rec) (make-record-info-obj-from-groupid-response rec))
	   groupid-responses))
