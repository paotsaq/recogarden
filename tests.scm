(include "api.scm")
(include "record-info.scm")
(include "tiddler.scm")
(include "mylogger.scm")
;(import test)
(import (only (chicken file)
			  create-temporary-file)
		(only (chicken file posix)
			  set-file-permissions!)
		(only (chicken time)
			  current-seconds))

(define TEST-SNATCHED-FILE "test-snatched-response.json")
(define TEST-IMAGE-FILE "capri-ri.jpg")
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

;(log-level ERROR)

; if test-file doesn't exist, loads the json response from a route into a variable
; intarweb#uri -> intarweb#response
; TODO abstract the save-obj-to-file function; it is not necessarily the same for all objects
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

(test-begin "API calls")
	(test "creates URIString with a single field"
	  #t (string=? "&id=1234" (create-field-string (list (list "id" "1234")))))
	(test "creates URIString with multiple fields"
	  #t (string=? "&id=1234&type=snatched" (create-field-string (list (list "id" "1234") (list "type" "snatched")))))
	(test "creates URI object from string"
		  #t (uri? (create-api-uri-object EXPECTED-SNATCHED-URI)))

	(test "can create EXPECTED-SNATCHED-URI"
	  #t (string=? EXPECTED-SNATCHED-URI
				   (build-uri-with-fields SNATCHED-URI
										  (list (list "id" (number->string USER-ID))
												(list "type" "snatched")
												(list "limit" (number->string 10))))))
	(test "can create EXPECTED-GROUP-URI-WITH-ID-1234"
	  #t (string=? EXPECTED-GROUP-URI-WITH-ID-1234
				   (build-uri-with-fields GROUP-URI
										  (list (list "id" (number->string 1234))))))

	(test "can create intarweb#request object"
		  #t (request? (make-request-to-endpoint API-KEY SNATCHED-URI)))
	(let ((start-time (current-seconds))
		  (response (get-response-from-endpoint INDEX-URI))
		  (end-time (current-seconds)))
		(test "HTTP request to API index is successful"
			  #t (string=? "success" (cdar (get-response-from-endpoint INDEX-URI))))
		(test "HTTP request waited a second"
			  #t (string=? "success" (cdar (get-response-from-endpoint INDEX-URI)))))
(test-end "API calls")

(test-begin "record-info object")
	; NOTE this should be loaded from a test file, too, akin to test-snatched-response.json
	(define groupid-responses (get-response-from-group-endpoint
								(get-uris-for-groupids-request
								  (get-groupids-from-records
									(get-records snatched-response)))))

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
				 (eq? (record-info-groupid test-record-info) 1234)
				 (string=? (record-info-image-uri test-record-info) EXPECTED-IMAGE-URI-WITH-ID-1234))))

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
(test-end "record-info object")

(test-begin "Tiddler creation")
	(define capri-ri (make-record-info title: "CAPRISONGS"
									   artist: "FKA Twigs"
									   year: 2022
									   groupid: 1678784
									   image-uri: TEST-IMAGE-URI))


	(test "creates a tiddler-content from record-info"
		  #t
		  (let ((timestamp (get-tiddler-timestamp)))
			   (string=? (create-tiddler-content capri-ri timestamp)
						 (string-append "created: " timestamp
									    "\ncreator: recogarden-script\n"
									 	"modified: " timestamp "\n"
									 	"modifier: recogarden-script\n"
									 	"tags: music-record\n"
									 	"title: CAPRISONGS\nauthor: FKA Twigs\n"
									 	"year: 2022\n"
										"album-art-tiddler: caprisongs-fka-twigs.jpg\n"
										"type: text/vnd.tiddlywiki"))))

	(let ((filepath "test-tiddler.tid")
		  (timestamp (get-tiddler-timestamp)))
	  (test "creates a file from a tiddler-content"
			filepath
			(begin (create-tiddler-file (create-tiddler-content capri-ri timestamp) filepath)
				   (and (and (file-exists? filepath) #t)
						; checking for file integrity
						(string=? (with-input-from-file filepath read-string)
								  (string-append "created: " timestamp
												 "\ncreator: recogarden-script\n"
												 "modified: " timestamp "\n"
												 "modifier: recogarden-script\n"
												 "tags: music-record\n"
												 "title: CAPRISONGS\n"
												 "author: FKA Twigs\n"
												 "year: 2022\n"
												 "album-art-tiddler: caprisongs-fka-twigs.jpg\n"
												 "type: text/vnd.tiddlywiki"))

						(delete-file* filepath)))))
	(test "error handling when tiddler file creation fails"
		#f
		(let ((temp-file (create-temporary-file)))
			(begin (set-file-permissions! temp-file 000)
					(create-tiddler-file (create-tiddler-content capri-ri (get-tiddler-timestamp)) temp-file))))

	(test "creates the album art filename for a record-info object"
		  #t
		  (string=? (get-album-art-filename capri-ri) "caprisongs-fka-twigs.jpg"))


	(let ((test-path (string-append "./"
									(get-album-art-filename capri-ri) ".")))
		(test "can save image file from the album-art request"
			  test-path
			  (begin (request-and-save-image-file (record-info-image-uri capri-ri) test-path)
					 ; NOTE should check for file integrity as well!
					 (and (and (file-exists? test-path) #t)
						  (delete-file* test-path)))))

	(test "creates a meta tiddler-content for a record-info"
		  #t
		  (let ((meta-tiddler-content (create-image-meta-tiddler capri-ri)))
		  ;NOTE there should be `is-tiddler-content` function,
		  ; that would parse a string and recognize a valid tiddler format.
		  (print meta-tiddler-content)
		  (and (string? meta-tiddler-content)
			   (string=? meta-tiddler-content
						 "title: caprisongs-fka-twigs.jpg\ntype: image/jpg\n"))))

	(test "routine for creation of album-art and meta-tiddler"
		  #t
		  (let ((test-path "./"))
			   (produce-record-info-album-art capri-ri test-path)))

(test-end "Tiddler creation")

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
			   ))
  (begin (append-groupid-to-logfile (record-info-groupid capri-ri) filepath)
		   (test "can find groupid in logfile"
				 #t
				 (record-exists? (record-info-groupid capri-ri) filepath))
		   (test "absent groupid retrieves false"
				 #f (record-exists? 1234 filepath))
		   (delete-file* filepath)))
