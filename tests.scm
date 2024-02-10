(include "api.scm")
(include "record-info.scm")
(include "tiddler.scm")

; API Tests
(test "can use a single field"
  #t (string=? "&id=1234" (create-field-string (list (list "id" "1234")))))
(test "can use two fields"
  #t (string=? "&id=1234&type=snatched" (create-field-string (list (list "id" "1234") (list "type" "snatched")))))
(test "creates URI object from string"
	  #t (uri? (create-api-uri-object EXPECTED-SNATCHED-URI)))

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

(test "can create intarweb#request object"
	  #t (request? (make-request-to-endpoint API-KEY SNATCHED-URI)))
(test "HTTP request to API index is successful"
	  #t (string=? "success" (cdar (get-response-from-endpoint INDEX-URI))))

; record-info

(test "can retrieve artist from solo album"
	  #t
	  (string=? (assemble-artist-string (retrieve-artists-from-group-response grouprecord-1234-response))
				"Kraftwerk"))
(test "can retrieve artists from group album"
	  #t
	  (string=? (assemble-artist-string (retrieve-artists-from-group-response grouprecord-1926431-response))
				"JPEGMAFIA, Danny Brown"))
(test "creating record-info struct from groupid-route-response"
	  #t
	  (let ((test-record-info (make-record-info-obj-from-groupid-response grouprecord-1234-response)))
		(and (record-info? test-record-info)
			 (string=? (record-info-artist test-record-info) "Kraftwerk")
			 (string=? (record-info-title test-record-info) "Trans-Europe Express (Trans Europa Express)")
			 (eq? (record-info-year test-record-info) 1977)
			 (eq? (record-info-groupid test-record-info) 1234))))

(test "can obtain vector of records"
	  #t
	  (vector? (get-records snatched-response)))
(test "creates a list of requests to groupId route from a list of groupId"
	  #t
	  (every string?
			(get-uris-for-groupids-request (get-groupids-from-records (get-records snatched-response)))))
 
(test "get a list of sucessful responses to groupId endpoint"
	  #t
	  (every (lambda (msg) (string=? "success" msg))
		     (map (lambda (response) (cdar response))
					   groupid-responses)))
(test "creates a list of record-info structs from a vector of records"
	  #t
	  (every record-info?
		(produces-record-info-list-from-vector-of-records groupid-responses)))

; tiddlers

(test "creates a tiddler-content from record-info"
	  #t
	  (let ((timestamp (get-timestamp)))
		   (string=? (create-tiddler-content capri-ri timestamp)
					 (string-append "created: " timestamp "\ncreator: recogarden-script\nmodified: " timestamp "\nmodifier: recogarden-script\ntags: music-record\ntitle: CAPRISONGS\nauthor: FKA Twigs\nyear: 2022\ntype: text/vnd.tiddlywiki"))))

(let* ((filepath "test-tiddler.tid"))
  (test "creates a file from a tiddler-content"
		filepath
		(begin (create-tiddler-file (create-tiddler-content capri-ri (get-timestamp)) filepath)
			   (and (and (file-exists? filepath) #t)
					(delete-file* filepath)))))

; append a new groupid to the logfile
; record-info -> 
(define (append-groupid-to-logfile groupid logfile)
  (call-with-output-file logfile
						 (lambda (output-port)
						   (write-line (number->string groupid) output-port))
						 #:append))

(let ((filepath "test-groupid.log"))
  (test "creates logfile with one single groupid"
		filepath
		(begin (append-groupid-to-logfile (record-info-groupid capri-ri) filepath)
			   (and (and (file-exists? filepath) #t)
					(string=? (number->string (record-info-groupid capri-ri))
							  (car (with-input-from-file filepath (lambda () (read-lines)))))
					(delete-file* filepath))))

  (test "creates logfile with one groupid and then appends another"
		filepath
		(begin (append-groupid-to-logfile (record-info-groupid capri-ri) filepath)
			   (and (and (file-exists? filepath) #t)
					(string=? (number->string (record-info-groupid capri-ri))
							  (car (with-input-from-file filepath (lambda () (read-lines))))))
			   (append-groupid-to-logfile 1234 filepath)
			   (let ((lines (with-input-from-file filepath (lambda () (read-lines)))))
				   (string=? (number->string (record-info-groupid capri-ri)) 
							 (car lines))
				   (string=? (number->string 1234) 
							 (cadr lines)))
			   (delete-file* filepath)
			   )))

(let ((filepath "test-groupid.log"))
	(begin (append-groupid-to-logfile (record-info-groupid capri-ri) filepath)
		   (test "can find groupid in logfile"
				 #t
				 (record-exists? (record-info-groupid capri-ri) filepath))
		   (test "absent groupid retrieves false"
				 #f (record-exists? 1234 filepath))
		   (delete-file* filepath)))
