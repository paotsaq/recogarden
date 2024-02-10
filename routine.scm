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

(define snatched-records (get-response-from-endpoint snatched-uri)) ; snatched-route-response 

; record-info creation from API calls
; snatched-route-response -> List[record-info]
(define record-info-list 
  (produces-record-info-list-from-vector-of-records
   (get-response-from-group-endpoint
	(get-uris-for-groupids-request
	  (get-groupids-from-records
		(get-records snatched-response))))))

(define PATH (string-append PAOGARDEN-PATH TIDDLERS-PATH))

; Tiddler creation from record-info list
(print "Sleeping a bit!")
(process-sleep 10)
(print "Slept all.")
(define timestamp (get-timestamp))
(define (create-tiddlers-from-record-info-list record-info-list)
  (let ((filtered-records
		 (filter (lambda (rec-info)
				   (not (record-exists? (record-info-groupid rec-info))))
		  record-info-list)))
	   (map (lambda (rec-info)
			  (begin (create-tiddler-file
					   (create-tiddler-content rec-info timestamp)
					   TIDDLERS-PATH)
					 (append-groupid-to-logfile
					   (record-info-groupid rec-info)
					   EXISTING-RECORDS)))
			filtered-records)
	   (print "Finished routine!
			  Added " (length filtered-records) " tiddlers.")))
