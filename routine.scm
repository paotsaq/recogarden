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

(get-records (get-response-from-endpoint snatched-uri))

; record-info creation from API calls
;  -> List[record-info]
(define record-info-list 
  (produces-record-info-list-from-vector-of-records
   (get-response-from-group-endpoint
	(get-uris-for-groupids-request
	  (get-groupids-from-records
		(get-records (get-response-from-endpoint snatched-uri)))))))

; Tiddler creation from record-info list
(define timestamp (get-tiddler-timestamp))
(define (create-tiddlers-from-record-info-list record-info-list)
  (let ((filtered-records
		 (filter (lambda (rec-info)
				   (not (record-exists? (record-info-groupid rec-info) EXISTING-RECORDS)))
		  record-info-list)))
	   (map (lambda (rec-info)
			  (begin (create-tiddler-file
					   (create-tiddler-content rec-info timestamp)
					   (string-append TIDDLERS-PATH "/" (record-info-title rec-info) ".tid"))
					 (produce-record-info-album-art rec-info TIDDLERS-PATH)
					 (append-groupid-to-logfile
					   (record-info-groupid rec-info)
					   EXISTING-RECORDS)))
			filtered-records)
	   ; NOTE this is not accurate
	   (print "Finished routine!
			  Added " (length filtered-records) " tiddlers.")))

(create-tiddlers-from-record-info-list record-info-list)
