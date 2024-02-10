(include "api.scm")
(include "record-info.scm")
(include "tiddler.scm")
(import (only (chicken process)
	      process-sleep))

; API jobs
(define snatched-uri
  (build-uri-with-fields SNATCHED-URI
						 (list (list "id" (number->string USER-ID))
							   (list "type" "snatched")
							   (list "limit" (number->string NBR-OF-SNATCHES)))))

; record-info creation from API calls
; snatched-route-response -> List[record-info]
(print "making API call")
(define record-info-list 
  (produces-record-info-list-from-vector-of-records
   (get-response-from-group-endpoint
	(get-uris-for-groupids-request
	  (get-groupids-from-records
		(get-records (get-response-from-endpoint snatched-uri)))))))

(define PATH (string-append PAOGARDEN-PATH TIDDLERS-PATH))

; Tiddler creation from record-info list
(define timestamp (get-timestamp))
(define (create-tiddlers-from-record-info-list record-info-list)
  (let ((filtered-records
		 (filter (lambda (rec-info)
				   (not (record-exists? (record-info-groupid rec-info) EXISTING-RECORDS)))
		  record-info-list)))
	   (map (lambda (rec-info)
			  (begin (create-tiddler-file
					   (create-tiddler-content rec-info timestamp)
					   (string-append TIDDLERS-PATH "/" (record-info-title rec-info) ".tid"))
					 (append-groupid-to-logfile
					   (record-info-groupid rec-info)
					   EXISTING-RECORDS)))
			filtered-records)
	   (print "Finished routine!
			  Added " (length filtered-records) " tiddlers.")))

(create-tiddlers-from-record-info-list record-info-list)
